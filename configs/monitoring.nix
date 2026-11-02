{
  config,
  lib,
  pkgs,
  ...
}:
{
  # creates a "localhost" vhost serving /nginx_status, restricted to 127.0.0.1. a
  # hand-rolled vhost does not work: nginx picks the lexicographically first vhost
  # as the implicit default server for 0.0.0.0:80, so a request for Host "localhost"
  # lands wherever that happens to point (alertmanager.kmein.r on makanek)
  services.nginx.statusPage = true;

  services.prometheus = {
    exporters = {
      nginx = {
        enable = config.services.nginx.enable; # inert on hosts without nginx
        listenAddress = "[::]"; # *.h resolves to ipv6 only
        # scrapeUri defaults to http://localhost/nginx_status, which is what
        # statusPage serves
      };
      node = {
        enable = true;
        listenAddress = "[::]";
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
        # the textfile collector is on by default but exports nothing without a
        # directory; this is where timers drop metrics an exporter cannot produce
        extraFlags = [ "--collector.textfile.directory=${pkgs.lib.niveum.textfileDirectory}" ];
      };
    };
  };

  # scraped from makanek over hyprspace, which carries only our own machines
  networking.firewall.interfaces.hyprspace.allowedTCPPorts = [
    config.services.prometheus.exporters.node.port
  ]
  ++ lib.optional config.services.nginx.enable config.services.prometheus.exporters.nginx.port;

  systemd.tmpfiles.rules = [
    (pkgs.lib.niveum.tmpfilesConfig {
      type = "d";
      path = pkgs.lib.niveum.textfileDirectory;
      mode = "0755";
      user = "root";
      group = "root";
    })
  ];

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
            if config.networking.hostName == "makanek" then "127.0.0.1" else "makanek.h"
          }:3100/loki/api/v1/push"
        }
      }
    '';
  };
}
