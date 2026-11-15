use std::collections::{BTreeMap, BTreeSet};
use std::os::fd::{FromRawFd, OwnedFd};
use std::process::Command;
use std::{env, fs, io};

use futures_util::{Stream, StreamExt, stream};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use zlink::{Reply, Server, introspect};

const STATE: &str = "/run/focus/state.json";

#[derive(Deserialize)]
struct Schedule {
    days: Vec<u32>,
    start: u32,
    end: u32,
    block: Vec<String>,
}

#[derive(Deserialize)]
struct Nft {
    off: String,
    offline: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Config {
    lists: BTreeMap<String, Vec<String>>,
    schedules: Vec<Schedule>,
    nft: Nft,
    nft_bin: String,
    systemctl: String,
}

#[derive(Deserialize, Serialize, Default, PartialEq)]
struct State {
    lists: Vec<String>,
    offline: bool,
}

unsafe extern "C" {
    fn tzset();
}

// iso weekday today and yesterday, and minutes since local midnight; localtime_r follows /etc/localtime, so dst is right
fn now() -> (u32, u32, u32) {
    unsafe {
        // localtime_r alone does not reread /etc/localtime
        tzset();
        let t = libc::time(std::ptr::null_mut());
        let mut tm: libc::tm = std::mem::zeroed();
        libc::localtime_r(&t, &mut tm);
        let today = if tm.tm_wday == 0 {
            7
        } else {
            tm.tm_wday as u32
        };
        let yesterday = if today == 1 { 7 } else { today - 1 };
        (today, yesterday, (tm.tm_hour * 60 + tm.tm_min) as u32)
    }
}

// days name the day a window starts; end <= start crosses midnight, end == start is 24 hours
fn active(s: &Schedule, (today, yesterday, t): (u32, u32, u32)) -> bool {
    if s.start < s.end {
        s.days.contains(&today) && s.start <= t && t < s.end
    } else {
        (s.days.contains(&today) && t >= s.start) || (s.days.contains(&yesterday) && t < s.end)
    }
}

// a corrupt file counts as missing rather than failing forever
fn load<T: DeserializeOwned>(path: &str) -> Option<T> {
    serde_json::from_slice(&fs::read(path).ok()?).ok()
}

fn store<T: Serialize>(path: &str, value: &T) -> io::Result<()> {
    let tmp = format!("{path}.tmp");
    fs::write(&tmp, serde_json::to_vec(value)?)?;
    fs::rename(tmp, path)
}

fn run(cmd: &str, args: &[&str]) -> Result<(), String> {
    match Command::new(cmd).args(args).status() {
        Ok(status) if status.success() => Ok(()),
        _ => Err(format!("{cmd} {args:?} failed")),
    }
}

fn blocked_names(cfg: &Config, lists: &[String]) -> Vec<String> {
    let names: BTreeSet<&String> = lists
        .iter()
        .filter_map(|l| cfg.lists.get(l))
        .flatten()
        .collect();
    names.into_iter().cloned().collect()
}

fn apply(cfg: &Config) -> Result<(), Box<dyn std::error::Error>> {
    let n = now();
    let mut blocked = BTreeSet::new();
    let mut offline = false;
    for s in cfg.schedules.iter().filter(|s| active(s, n)) {
        for b in &s.block {
            if b == "offline" {
                offline = true;
            } else {
                blocked.insert(b.clone());
            }
        }
    }

    // every time, so a firewall flush by someone else is repaired at the next edge
    let file = if offline {
        &cfg.nft.offline
    } else {
        &cfg.nft.off
    };
    run(&cfg.nft_bin, &["-f", file])?;

    fs::create_dir_all("/run/focus")?;
    let previous: Option<State> = load(STATE);
    let state = State {
        lists: blocked.into_iter().collect(),
        offline,
    };
    store(STATE, &state)?;

    // the hook reads the state only at start; if it is not running, socket activation reads it later
    if previous.as_ref() != Some(&state) {
        run(&cfg.systemctl, &["try-restart", "focus-hook.service"])?;
    }
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, introspect::Type)]
struct ResourceKey {
    name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, introspect::Type)]
struct Question {
    key: ResourceKey,
}

#[derive(Debug, Serialize, Deserialize, introspect::Type)]
struct Filter {
    #[serde(rename = "filterDomains")]
    filter_domains: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, introspect::Type)]
struct Verdict {
    #[serde(skip_serializing_if = "Option::is_none")]
    rcode: Option<i64>,
}

struct Hook {
    names: Vec<String>,
}

#[zlink::interface("io.systemd.Resolve.Hook")]
impl Hook {
    // an empty filter disables the hook in resolved; a final reply would drop the filter and send every lookup here
    #[zlink(more)]
    async fn query_filter(&self, more: bool) -> impl Stream<Item = Reply<Filter>> + Unpin {
        let first = stream::once(std::future::ready(
            Reply::new(Some(Filter {
                filter_domains: self.names.clone(),
            }))
            .set_continues(Some(more)),
        ));
        if more {
            first.chain(stream::pending()).left_stream()
        } else {
            first.right_stream()
        }
    }

    // the filter is only advisory, and resolved (as of v259) drops it for good once it has
    // reconnected after a restart of the hook, sending every lookup here; so check again
    async fn resolve_record(&self, question: Vec<Question>) -> Verdict {
        let blocked = question.iter().any(|q| {
            let n = q.key.name.trim_end_matches('.').to_lowercase();
            self.names
                .iter()
                .any(|d| n == *d || n.ends_with(&format!(".{d}")))
        });
        Verdict {
            rcode: blocked.then_some(3),
        }
    }
}

#[derive(zlink::Service)]
#[zlink(vendor = "niveum", product = "focus", version = "1")]
struct HookService(Hook);

fn hook(cfg: &Config) -> Result<(), Box<dyn std::error::Error>> {
    if env::var("LISTEN_FDS").as_deref() != Ok("1") {
        return Err("focus hook: expected one socket from systemd".into());
    }
    let names = blocked_names(cfg, &load::<State>(STATE).unwrap_or_default().lists);
    eprintln!("focus hook: blocking {} domains", names.len());
    zlink::zruntime::block_on(async {
        let listener =
            zlink::zruntime::unix::Listener::try_from(unsafe { OwnedFd::from_raw_fd(3) })?;
        Server::new(listener, HookService(Hook { names }))
            .run()
            .await?;
        Ok(())
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let [_, path, mode] = args.as_slice() else {
        return Err("usage: focus CONFIG apply|hook".into());
    };
    let cfg: Config = serde_json::from_slice(&fs::read(path)?)?;
    match mode.as_str() {
        "apply" => apply(&cfg),
        "hook" => hook(&cfg),
        _ => Err("usage: focus CONFIG apply|hook".into()),
    }
}
