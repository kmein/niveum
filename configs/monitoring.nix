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

  systemd.services.promtail = {
    description = "Promtail service for Loki";
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = ''
        ${pkgs.grafana-loki}/bin/promtail --config.file ${
          (pkgs.formats.yaml { }).generate "promtail.yaml" {
            server = {
              http_listen_port = 28183;
              grpc_listen_port = 0;
            };
            positions.filename = "/tmp/positions.yaml";
            clients = [
              {
                url = "http://${
                  if config.networking.hostName == "makanek" then "127.0.0.1" else "makanek.r"
                }:3100/loki/api/v1/push";
              }
            ];
            scrape_configs = [
              {
                job_name = "journal";
                journal = {
                  max_age = "12h";
                  labels.job = "systemd-journal";
                  labels.host = config.networking.hostName;
                };
                relabel_configs = [
                  {
                    source_labels = [ "__journal__systemd_unit" ];
                    target_label = "unit";
                  }
                ];
              }
            ];
          }
        }
      '';
    };
  };
}
