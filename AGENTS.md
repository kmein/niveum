# niveum — Agent Notes

## What This Is

A NixOS flake managing desktops, servers and family laptops for one user (kmein/kfm).
Levantine food-themed hostnames: fatteh, makanek, manakish, ful, mouneh.

## Repository Structure

```
flake.nix          # ~510 lines — inputs, nixosConfigurations, apps, packages output
overlays/          # default overlay, split by provenance: packages, unstable, inputs, wrappers, lib
configs/           # ~50 NixOS config fragments imported by systems
  default.nix      # 200+ line mega-module for desktop machines (user, shell, gnupg, i18n, etc.)
  graphical/       # Hyprland + home-manager config (415 lines in home-manager.nix)
  packages.nix     # ~250 lines of environment.systemPackages
  bots/            # Telegram/Matrix bot configs
  keyboard/        # XKB layouts (Coptic, Avestan, Gothic, etc.)
configs/*.nix      # Individual concerns: bluetooth, sound, printing, ssh, fonts, etc.
modules/           # Proper NixOS modules with options (telegram-bot, go-webring, mail, etc.)
wrapperModules/    # lassulus/wrappers modules: program + config in store, flake output `wrapperModules`
packages/          # ~107 package files; default.nix is the name -> callPackage registry
systems/<name>/    # Per-machine: configuration.nix + hardware-configuration.nix + extras
tests/             # NixOS VM tests, exposed as flake checks (tests/mail.nix → checks.{x86_64,aarch64}-linux.mail)
lib/               # default.nix (niveum helpers + constants), machines.nix (IP/key inventory)
secrets/           # agenix-encrypted .age files (empty dir in checkout, tracked via secrets.txt)
docs/              # procedures done by hand, outside any deploy: adding and retiring a machine, disaster recovery
```

## Key Relationships

- **niphas** (input): Provides shared "how I like things" config — nixosModules (shell, editor, git, desktop, nix, udiskie) exposing `niphas.*` options (deep-merging `settings`, swappable `<tool>.package`, defaults at `mkDefault`/`mkOverride 900` priority). Personal overrides live in `configs/niphas.nix` (profiles.desktop); tool packages are referenced as `config.niphas.<tool>.package` (there is no `niphas-*` overlay anymore).
- **configs/default.nix**: The "big desktop profile" — imported by fatteh, manakish (the main desktop machines). NOT imported by servers or family laptops. It imports `configs/desktop-core.nix`, the part that needs neither secrets nor an entry in `lib/machines.nix` and stays light; anything secret-backed or heavy goes into `default.nix`, not the core.
- **profiles** (in flake.nix): `profiles.base` (no secrets, no host inventory), `profiles.default` (base + agenix, sshd, retiolum, hyprspace), `profiles.desktop`, `profiles.server` — lists of modules composed per machine. `configs/sshd.nix` (with its ssh onion service) reads the host's `lib/machines.nix` entry, so it stays out of base.
- **mouneh**: niveum on a LUKS-encrypted USB stick, for any x86_64 UEFI machine, doubling as installer and disaster-recovery kit. `profiles.base` + `profiles.desktop` + desktop-core: no secrets, no sshd, not in `lib/machines.nix` (so no deploy app), so it evaluates without the secrets submodule and updates itself with `nixos-rebuild switch --flake github:kmein/niveum#mouneh`. Autologin without password, sudo without password, no screen lock: the LUKS passphrase is the only gate. `nix run .#mouneh-install -- /dev/sdX` partitions and encrypts a fresh stick (partlabels `mouneh-boot`/`mouneh-crypt`), or adds a generation to an existing one.
- **lib.niveum**: Custom lib injected via overlay (`pkgs.lib.niveum`) — used everywhere for machine addresses, SSH port, helper functions.
- **modules/mail.nix** (`nixosModules.mail`, imported by configs/mail.nix and tests/mail.nix): plain NixOS module, no home-manager. Accounts live in `niveum.mail.users.<user>.accounts` (configs/mail.nix, configs/uni.nix). Each user gets `aerc` (local Maildir + msmtpq outbox, works offline), `aerc-online` (direct IMAP/SMTP escape hatch), `mail-sync` (flush outbox, two-way mbsync, notmuch new; run every 5 min by the `mail-sync` user timer) and `msmtp-queue`. Programs are configured through `wrapperModules/` plus upstream `wrappers.wrapperModules.notmuch`; configs are store files passed as flags, except msmtprc at `/etc/niveum-mail/<user>/msmtprc`, because queued mails keep its path. Verify mail changes with `nix build '.?submodules=1#checks.x86_64-linux.mail'`.
- **configs/agents/** (desktop profile): `instructions.md` and in-repo skills become `~/.claude/CLAUDE.md` and `~/.claude/skills/<name>` via `programs.claude-code`. omp reads the same files through `enabledProviders: [claude]`, set in `wrapperModules.omp`'s `settings`: they become a `PI_CONFIG_FILES` overlay, which outranks `~/.omp/agent/config.yml` and is never written back, so omp settings that must always hold go there. Proprietary skills stay unmanaged in `~/.claude/skills`, out of this public repo. `~/.claude/settings.json` and omp's `config.yml` are rewritten at runtime and stay unmanaged.
- **Agent sandbox**: `omp` and `claude` are wrappers (configs/agents/) that run the agent in sbox, rooted at the git checkout they are started in (or, given `--here` as first argument, at `$PWD` itself, checkout or not; never at a directory containing `$HOME`), with only their own state dirs and a read-only gh login, no ssh/gpg agent, no `/run/agenix`, and no route to the LAN or the overlays. They share the user's uid, so no desktop user may be in `nix.settings.trusted-users` (a trusted user is root via the daemon); caches the agents need go in `nix.settings.extra-substituters`. Commits made inside are unsigned; re-sign with `git rebase -S` before pushing. `configs/agents/instructions.md` tells the agents themselves about the sandbox, including how to push without ssh.
- **Host placement**: makanek is the vault, ful the yolo box. A service goes on makanek only if losing or leaking its data or credentials would hurt and strangers can neither write to it nor run things on it; everything else (toys, anonymous editing, PHP apps, ssh shells, community services, bots) goes on ful. A service keeps its exposure when it moves. makanek is the vault because it is paid, sits next to the storagebox and has to outlive ful to alert about it. Both still share the `server` restic repository, so a compromised ful can delete makanek's backups; only the storagebox snapshots protect against that.
- **Backups**: restic over sftp to the Hetzner storagebox, one sub-account and password per group (`personal`: desktops; `server`: makanek, ful), so a compromised server can neither read nor delete the personal repository. sftp has no append-only mode; the storagebox's own snapshots (Hetzner Console) are what survives a wiped repository. `lib.niveum.restic` maps each host to its group and names the one host per group that runs forget/prune/check. Every client imports `configs/restic-client.nix` (desktops through `configs/backup.nix`); on scraped hosts it records the last successful backup for the `BackupStale` alert. The storagebox host key is pinned, ssh runs with `StrictHostKeyChecking=yes`. Recovering from the loss of everything at home (paper key, off-site disk) is `docs/disaster-recovery.md`.
- **SSH host keys**: `hostKey` in `lib/machines.nix`, the same keys `secrets/secrets.nix` encrypts to. `configs/ssh.nix` (desktops) pins them for every name and address that leads to a host (its `addresses`), with `StrictHostKeyChecking yes` and `~/.ssh/known_hosts` ignored; a route missing from `addresses` (new address or DNS suffix) falls back to trust on first use.
- **Git hosting** (`systems/makanek/git.nix`, code.kmein.de): bare repositories in `/var/lib/git`, owned by a `git` user whose shell is `git-shell` and whose keys are `lib.niveum.kieran.sshKeys`, so every key there can read and write every repository. The desktops' ssh config gives `code.kmein.de` that user and port, so remotes are `code.kmein.de:<name>.git` (elsewhere `ssh://git@code.kmein.de:<sshPort>/~/<name>.git`). `ssh code.kmein.de repo-new <name>` creates a repository: git-shell runs only what `git-shell-commands` in its home holds. Two cgit instances read the same repositories: code.kmein.de, behind Anubis, shows and serves only the `public` set; private.code.kmein.de shows and serves every repository, the whole vhost behind the code password (`code-basicAuth.age`, an htpasswd file). So a new repository starts private and becomes public by being added to `public`. nginx rewrites gitea's old `/<owner>/<name>` urls on code.kmein.de: git is served in place, browsers are redirected to the cgit page.
- **Mirrors** (`mirrors` in `systems/makanek/git.nix`): repositories pushed to GitHub, fetched from there hourly by `git-mirror`, which creates each on its first run and follows upstream's default branch. The fetch force-updates every branch, so push changes to GitHub, never to the mirror; list a mirror in `public` too if cgit should show it.
- **WireGuard** (`configs/wireguard.nix`, `profiles.default`): interface `niveum` on the hosts with a `wireguard` entry in `lib/machines.nix`, keys in `<host>-wireguard-privateKey.age`. Every host peers with each one that has an `externalIp`; laptops don't peer with each other, since neither knows the other's endpoint. It is only a gate for private vhosts, not a replacement for retiolum or hyprspace: a name in `lib.niveum.wireguard.names.<host>` resolves to that host's WireGuard address on every peer (`networking.hosts`), and its vhost must carry `extraConfig = pkgs.lib.niveum.wireguardOnly`, which an assertion checks. ful's address is a peer but not in that allowlist, since ful runs strangers' code. The names keep public A records only for Let's Encrypt's HTTP-01, which reaches the plain-http server that carries no vhost `extraConfig`.
- **IRC** (`systems/makanek/soju.nix`, `configs/irc.nix`): soju on makanek, reachable only over retiolum as `makanek.kmein.r:6697`, with the ca.r certificate nginx already holds for that name. Networks and channels are declared in `soju.nix`; `soju-setup` applies them with sojuctl on every start and leaves whatever a client added. SASL passwords live in `soju-sasl.age`, one `<network> <password>` line each. The desktops run senpai as `kmein@<hostname>`, since soju keeps read markers and history per client name.
- **Offline docs** (`configs/kiwix/`, desktop profile): `services.kiwix-serve` on `127.0.0.1:8095`, proxied by a loopback-only nginx at `http://docs.localhost` (not `.local`: `mdns4_minimal [NOTFOUND=return]` precedes `files`), serves ZIMs (DevDocs, ArchWiki, Gentoo wiki, …), pinned in `zims.json` (key = ZIM name without date) and fetched with `fetchurl`, so they live in the store and need no backup. CI keeps them off cachix (`pushFilter`), since kiwix's mirrors serve them anyway. kiwix-serve rejects a full-text search across books of different languages, so a search has to be narrowed to one language. Bump all pins with `nix run .#kiwix-update` (resolves download.kiwix.org's `<name>.zim` alias to the newest dated file and its `.sha256`); to add a ZIM, add `"<name>": {}` and run it. ZIMs not on download.kiwix.org are pinned outside `zims.json`, since kiwix-update would fail to resolve them: the Typst docs (a community zimit snapshot kept at `pkgs.typst`'s version) by hand in `default.nix`, and the RFCs in `rfc.json`. Kiwix publishes no RFCs, so `nix run .#rfc-zim` builds a ZIM from rfc-editor.org's rsync, uploads it to c.r under a dated name and rewrites `rfc.json`; it needs retiolum, so it cannot run in the agent sandbox.
- **Text to speech** (`configs/tts/`, desktop profile): speech-dispatcher's only module is piper, the engine calibre reads aloud with, so Firefox's `speechSynthesis` (reader view's narrate, any page using the Web Speech API) and `spd-say` speak with piper voices. sd_generic would start piper per sentence, which takes longer than synthesising it, so a `piper` user service keeps an http server with the voices loaded on `127.0.0.1:10200`, and the module only posts text to it and plays the WAV with `pw-play`. Voices are pinned by hash in `voices` (named as on https://huggingface.co/rhasspy/piper-voices, plus their language and speechd voice type). sd_generic reads float options as hundredths, hence the rate arithmetic in jq. piper comes from unstable (`piper-tts-python` in `overlays/unstable.nix`), since Hebrew voices need piper ≥ 1.5. Anki has no TTS player on Linux, so `anki.py` is an add-on, and Anki is installed here as `anki.withAddons`. That points Anki at a store directory instead of `addons21` and disables the add-on dialog, so every add-on goes through `withAddons`, never AnkiWeb. The add-on offers every piper voice for its locale, matched by language when a deck names another country (`ar_SA` gets `ar_JO`) or a voice that doesn't exist (`voices=AwesomeTTS`), and reads the locales in `espeakLocales`, for which piper has no voice, with espeak-ng. Most templates mark speech with AnkiDroid's `<tts>` markup instead of `{{tts}}`, which desktop Anki ignores; the add-on turns it into TTS tags while the card renders, so the collection stays as AnkiDroid reads it. A bare `<tts>` names no language (AnkiDroid asks per deck), so `ankiDecks` gives it per top-level deck; a deck missing there stays silent. A deck whose templates mark nothing but share their note type with other decks (`Einseitig + Beispiel`) names its `fields` there instead, since marking the template would make the other decks speak too. Text that holds one of my own recordings (`[sound:…]`) is not read. The collection's `shell.nix` in `~/.local/share/Anki2` pins nixpkgs to this flake's revision, so its library matches the desktop's `anki`.

## Coding Conventions

- Packages use `writers.writeDashBin`, `writers.writeBashBin`, or `writers.writePython3Bin`
- Dependencies are referenced via `lib.getExe pkg` (main executable) or `lib.getExe' pkg "name"` (specific binary)
- For packages needing many commands via PATH, use `lib.makeBinPath` instead (see `packages/prospekte.nix`)
- To add a package: create `packages/<name>.nix`, register it in `packages/default.nix` — the overlay and the flake `packages` output both derive from that map
- The base web domain is `pkgs.lib.niveum.domain` (`kmein.de`); build subdomains as `"pad.${pkgs.lib.niveum.domain}"`, never as literals
- Server-shared fragments live in configs/ and are imported from a host's configuration.nix: `nginx.nix` (recommended settings + ACME), `restic-client.nix` (backup repo/timer/secrets; the desktops get it through `backup.nix`), `server-packages.nix`
- An nginx vhost named `*.r` gets `enableACME` and `forceSSL` from `configs/nginx.nix`, with the certificate from krebs' `ca.r` (step-ca, name-constrained to `.r`/`.w`; every host trusts it through `kartei.nixosModules.ca`). ca.r reaches the vhost by kartei's alias list, so a `.r` name must be listed under the host that serves it, or its certificate cannot be issued
- Per-service backup paths are added via `services.restic.backups.niveum.paths = [ ... ]` in the service's own file; retiolum key secrets are derived from `networking.hostName` in configs/retiolum.nix
- To verify a change, eval the affected hosts: `nix eval --raw '.?submodules=1#nixosConfigurations.<host>.config.system.build.toplevel.drvPath'` — new files must be `git add`ed first or flake eval won't see them; after adding/removing secrets run `nix run .#mock-secrets` to refresh secrets.txt

## Known Bugs / Broken References

None currently known. No NIX_PATH (`<...>`) lookups remain; the flake evaluates purely (with `?submodules=1`).

## Architectural Issues

### 1. configs/default.nix is a grab-bag (200+ lines, ~15 inline anonymous modules)

It's a list of `imports` mixing inline `{ ... }` blocks with file imports. Hard to find what's defined where.

### 2. The `pkgs.lib.niveum` pattern

Custom lib injected via overlay into `pkgs.lib`. Unconventional — only available where overlay is applied. A `specialArgs` approach or standalone lib would be cleaner.

### 3. configs/ vs modules/ distinction blurry

`configs/` has both stateless config fragments (spacetime.nix = timezone) and stateful ones (backup.nix, cloud.nix). `modules/` has proper option-declaring modules. Some configs/ files import from modules/.

## Machines Overview

| Machine  | Role         | Profile         | Arch    | Notes                                                           |
| -------- | ------------ | --------------- | ------- | --------------------------------------------------------------- |
| fatteh   | Desktop      | default+desktop | x86_64  | ThinkPad T480, CUDA, main daily                                 |
| manakish | Desktop      | default+desktop | x86_64  | ThinkPad X220                                                   |
| ful      | Server       | default+server  | aarch64 | Oracle, 2 cores, 12 GB RAM, no swap; the yolo box               |
| makanek  | Server       | default+server  | x86_64  | Hetzner, 2 cores, 4 GB RAM, no swap; the vault                  |
| mouneh   | USB stick    | base+desktop    | x86_64  | any UEFI machine; installer and recovery kit                    |

## Remaining Improvement Ideas

1. **Break up configs/default.nix** into proper named files
2. **Comment-disabled files** (configs/i3.nix, configs/bots/tlg-wotd.nix, makanek's menstruation/names/onlyoffice.nix) — decide to reenable or delete
