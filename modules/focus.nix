# scheduled blocking: sites through a systemd-resolved hook, which answers nxdomain
# for the listed domains; offline through an nft table that rejects everything
# outside the lan. a timer runs an idempotent apply program at every window edge,
# since nft's own time matching mixes utc hours with local weekdays and breaks
# around dst and midnight.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.niveum.focus;
  inherit (lib) types;

  time = types.strMatching "([01][0-9]|2[0-3]):[0-5][0-9]";

  offlineRules = ''
    oifname "lo" accept
    ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, 169.254.0.0/16, 224.0.0.0/4, 255.255.255.255 } accept
    ip6 daddr { fc00::/7, fe80::/10, ff00::/8 } accept
    icmpv6 type { nd-router-solicit, nd-neighbor-solicit, nd-neighbor-advert } accept
    meta l4proto tcp reject with tcp reset
    reject with icmpx admin-prohibited
  '';
  table = rules: ''
    table inet focus {
      chain output {
        type filter hook output priority filter; policy accept;
        ${rules}
      }
      chain forward {
        type filter hook forward priority filter; policy accept;
        ${rules}
      }
    }
  '';
  # add-then-delete makes every file replace the table in one transaction, even when it is absent
  nftFile =
    name: body:
    pkgs.writeText "focus-${name}.nft" ''
      table inet focus {}
      delete table inet focus
      ${body}
    '';

  dayNumbers = {
    Mon = 1;
    Tue = 2;
    Wed = 3;
    Thu = 4;
    Fri = 5;
    Sat = 6;
    Sun = 7;
  };
  # toInt rejects the leading zero in "09"
  minutes =
    t: lib.toIntBase10 (builtins.substring 0 2 t) * 60 + lib.toIntBase10 (builtins.substring 3 2 t);

  configFile = pkgs.writeText "focus.json" (
    builtins.toJSON {
      lists = lib.mapAttrs (_: map lib.toLower) cfg.lists;
      schedules = map (s: {
        days = map (d: dayNumbers.${d}) s.days;
        start = minutes s.from;
        end = minutes s.until;
        inherit (s) block;
      }) cfg.schedules;
      nft = {
        off = "${nftFile "off" ""}";
        offline = "${nftFile "offline" (table offlineRules)}";
      };
      nftBin = lib.getExe' pkgs.nftables "nft";
      systemctl = lib.getExe' config.systemd.package "systemctl";
    }
  );

  apply = lib.getExe cfg.package;

  sleepTargets = [
    "suspend.target"
    "hibernate.target"
    "hybrid-sleep.target"
    "suspend-then-hibernate.target"
  ];
in
{
  options.niveum.focus = {
    enable = lib.mkEnableOption "scheduled blocking of websites and the whole internet";
    package = lib.mkPackageOption pkgs "focus" { };
    lists = lib.mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
      description = "Named lists of domains. A domain blocks itself and every subdomain.";
    };
    schedules = lib.mkOption {
      default = [ ];
      description = "Windows during which lists of domains, or the whole internet, are blocked.";
      type = types.listOf (
        types.submodule {
          options = {
            days = lib.mkOption {
              type = types.nonEmptyListOf (
                types.enum [
                  "Mon"
                  "Tue"
                  "Wed"
                  "Thu"
                  "Fri"
                  "Sat"
                  "Sun"
                ]
              );
              description = "Days the window starts on.";
            };
            from = lib.mkOption {
              type = time;
              description = "Local time the window starts at.";
            };
            until = lib.mkOption {
              type = time;
              description = "If until is not after from, the window ends the next day, so until = from gives a 24-hour window (00:00 to 00:00 on every day means always on).";
            };
            block = lib.mkOption {
              type = types.nonEmptyListOf types.str;
              description = ''Names from lists, or "offline" for the whole internet.'';
            };
          };
        }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.resolved.enable;
        message = "niveum.focus blocks sites through systemd-resolved hooks; enable services.resolved";
      }
      {
        assertion = lib.versionAtLeast config.systemd.package.version "259";
        message = "niveum.focus needs systemd-resolved hooks, which systemd 259 introduced";
      }
      {
        assertion = !(cfg.lists ? offline);
        message = ''niveum.focus.lists: "offline" is reserved for blocking the whole internet'';
      }
      {
        assertion = !(config.networking.nftables.enable && config.networking.nftables.flushRuleset);
        message = "niveum.focus: networking.nftables.flushRuleset would drop table inet focus on every firewall reload";
      }
    ]
    ++ map (b: {
      assertion = b == "offline" || cfg.lists ? ${b};
      message = "niveum.focus.schedules: unknown list ${b}";
    }) (lib.unique (lib.concatMap (s: s.block) cfg.schedules));

    # resolved runs as systemd-resolve and has to connect; it finds the socket by the directory's mtime
    systemd.sockets.focus-hook = {
      description = "focus resolve hook socket";
      wantedBy = [ "sockets.target" ];
      listenStreams = [ "/run/systemd/resolve.hook/focus" ];
      socketConfig = {
        SocketGroup = "systemd-resolve";
        SocketMode = "0660";
      };
    };

    # bursts of clock changes and resume each apply, and each changed state restarts the
    # hook; the default start limit would leave either failed until the next boot
    systemd.services.focus-hook = {
      description = "focus resolve hook";
      startLimitIntervalSec = 0;
      serviceConfig = {
        ExecStart = "${apply} ${configFile} hook";
        Restart = "on-failure";
        RestartSec = "1s";
      };
    };

    systemd.services.focus = {
      description = "apply focus schedules";
      startLimitIntervalSec = 0;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${apply} ${configFile} apply";
      };
      # reapply after resume
      wantedBy = sleepTargets;
      after = sleepTargets;
    };

    systemd.timers.focus = {
      wantedBy = [ "timers.target" ];
      restartTriggers = [ configFile ];
      timerConfig = {
        OnActiveSec = "1s";
        OnCalendar = map (t: "*-*-* ${t}:00") (
          lib.unique (
            lib.concatMap (s: [
              s.from
              s.until
            ]) cfg.schedules
          )
        );
        OnClockChange = true;
        OnTimezoneChange = true;
        # the default of a minute would delay the edges
        AccuracySec = "1s";
      };
    };
  };
}
