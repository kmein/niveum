{
  config,
  pkgs,
  ...
}:
{
  services.nginx.virtualHosts.default = {
    locations."= /stub_status".extraConfig = "stub_status;";
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      nginx.enable = false;
      node = {
        enable = true;
        openFirewall = true;
        enabledCollectors = [
          "conntrack"
          "diskstats"
          "entropy"
          "filefd"
          "filesystem"
          "loadavg"
          "mdadm"
          "meminfo"
          "netdev"
          "netstat"
          "stat"
          "time"
          "vmstat"
          "systemd"
          "logind"
          "interrupts"
          "ksmd"
        ];
        port = 9002;
      };
    };
  };

  services.alloy = {
    enable = true;
    configPath = pkgs.writeText "config.alloy" ''
      loki.relabel "journal" {
        forward_to = []
        rule {
          source_labels = ["__journal__systemd_unit"]
          target_label  = "unit"
        }
      }

      loki.source.journal "journal" {
        max_age       = "12h"
        relabel_rules = loki.relabel.journal.rules
        forward_to    = [loki.write.local.receiver]
        labels = {
          job  = "systemd-journal",
          host = "${config.networking.hostName}",
        }
      }

      loki.write "local" {
        endpoint {
          url = "http://${
            if config.networking.hostName == "makanek" then "127.0.0.1" else "makanek.r"
          }:3100/loki/api/v1/push"
        }
      }
    '';
  };
}
