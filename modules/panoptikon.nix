{
  config,
  lib,
  pkgs,
  ...
}: {
  options.services.panoptikon = {
    enable = lib.mkEnableOption "Generic command output / website watcher";
    watchers = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule (watcher: {
        options = {
          script = lib.mkOption {
            type = lib.types.path;
            description = ''
              A script whose stdout is to be watched.
            '';
            example = ''
              pkgs.writers.writeDash "github-meta" '''
                ''${pkgs.curl}/bin/curl -sSL https://api.github.com/meta | ''${pkgs.jq}/bin/jq
              '''
            '';
          };
          frequency = lib.mkOption {
            type = lib.types.str;
            description = ''
              How often to run the script. See systemd.time(7) for more information about the format.
            '';
            example = "*:0/3";
            default = "daily";
          };
          loadCredential = lib.mkOption {
            type = lib.types.listOf lib.types.string;
            description = ''
              This can be used to pass secrets to the systemd service without adding them to the nix store.
            '';
            default = [];
          };
          reporters = lib.mkOption {
            type = lib.types.listOf lib.types.path;
            description = ''
              A list of scripts that take the diff (if any) via stdin and report it (e.g. to IRC, Telegram or Prometheus). The name of the watcher will be in the $PANOPTIKON_WATCHER environment variable.
            '';
            example = ''
              [
                (pkgs.writers.writeDash "telegram-reporter" '''
                  ''${pkgs.curl}/bin/curl -X POST https://api.telegram.org/bot''${TOKEN}/sendMessage \
                    -d chat_id=123456 \
                    -d text="$(cat)"
                ''')
                (pkgs.writers.writeDash "notify" '''
                  ''${pkgs.libnotify}/bin/notify-send "$PANOPTIKON_WATCHER has changed."
                ''')
              ]
            '';
          };
        };
        config = {};
      }));
    };
  };

  config = let
    cfg = config.services.panoptikon;
  in
    lib.mkIf cfg.enable {
      users.extraUsers.panoptikon = {
        isSystemUser = true;
        createHome = true;
        home = "/var/lib/panoptikon";
        group = "panoptikon";
      };

      users.extraGroups.panoptikon = {};

      systemd.timers = lib.attrsets.mapAttrs' (watcherName: _:
        lib.nameValuePair "panoptikon-${watcherName}" {
          timerConfig.RandomizedDelaySec = toString (60 * 60);
        })
      cfg.watchers;

      systemd.services =
        {
          setup-panoptikon = {
            enable = true;
            wantedBy = ["multi-user.target"];
            serviceConfig = {
              Type = "oneshot";
              User = "panoptikon";
              Group = "panoptikon";
              WorkingDirectory = "/var/lib/panoptikon";
              Restart = "on-failure";
              StartLimitBurst = 5;
              RestartSec = 30;
            };
            script = ''
              ${pkgs.git}/bin/git init --quiet
              ${pkgs.git}/bin/git config user.email "panoptikon@${config.networking.hostName}"
              ${pkgs.git}/bin/git config user.name Panoptikon
            '';
          };
        }
        // lib.attrsets.mapAttrs' (watcherName: watcherOptions:
          lib.nameValuePair "panoptikon-${watcherName}" {
            enable = true;
            after = ["setup-panoptikon.service"];
            startAt = watcherOptions.frequency;
            serviceConfig = {
              Type = "oneshot";
              User = "panoptikon";
              Group = "panoptikon";
              WorkingDirectory = "/var/lib/panoptikon";
              RestartSec = toString (60 * 60);
              Restart = "on-failure";
              LoadCredential = watcherOptions.loadCredential;
            };
            unitConfig = {
              StartLimitIntervalSec = "300";
              StartLimitBurst = "5";
            };
            environment.PANOPTIKON_WATCHER = watcherName;
            wants = ["network-online.target"];
            script = ''
              set -efu

              ${watcherOptions.script} > ${watcherName}

              diff_output=$(${pkgs.diffutils}/bin/diff --new-file ${watcherName}.old ${watcherName})

              if [ -n "$diff_output" ]; then
                ${lib.strings.concatMapStringsSep "\n" (reporter: ''
                  echo "$diff_output" | ${reporter}
                '') watcherOptions.reporters}
                :
              fi

              mv ${watcherName} ${watcherName}.old
            '';
          })
        cfg.watchers;
    };
}
