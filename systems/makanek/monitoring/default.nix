{
  lib,
  config,
  pkgs,
  ...
}: let
  lokiConfig = import ./loki.nix;
  blackboxConfig = import ./blackbox.nix;
  inherit (import ../../../lib) restic;
in {
  services.grafana = {
    enable = true;
    settings = {
      server = {
        domain = "grafana.kmein.r";
        http_port = 9444;
        http_addr = "127.0.0.1";
      };
      dashboards.default_home_dashboard_path = toString ./grafana-dashboards/niveum.json;
      security = {
        admin_user = "admin";
        admin_password = "$__file{${config.age.secrets.grafana-password-admin.path}}";
      };
    };
    provision = {
      enable = true;
      dashboards.settings.providers = [
        {
          name = "dashboards";
          type = "file";
          options.path = ./grafana-dashboards;
        }
      ];
      datasources.settings.datasources = builtins.fromJSON (builtins.readFile ./grafana-datasources.json);
    };
  };

  services.nginx.virtualHosts = {
    ${config.services.grafana.settings.server.domain} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
        proxyWebsockets = true;
      };
    };
    ${lib.removePrefix "http://" config.services.prometheus.alertmanager.webExternalUrl} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.prometheus.alertmanager.port}";
        proxyWebsockets = true;
      };
    };
    ${lib.removePrefix "http://" config.services.prometheus.webExternalUrl} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.prometheus.port}";
        proxyWebsockets = true;
      };
    };
  };

  services.prometheus.webExternalUrl = "http://prometheus.kmein.r";

  niveum.passport.services = [
    {
      title = "Prometheus";
      link = config.services.prometheus.webExternalUrl;
      description = "collects metrics from devices in the <i>niveum</i> network, blackbox monitors some websites.";
    }
    {
      title = "Loki";
      description = "aggregates logs of the <i>niveum</i> network.";
    }
    {
      title = "Grafana";
      link = "http://${config.services.grafana.settings.server.domain}";
      description = "displays metrics from devices in the <i>niveum</i> network.";
    }
    {
      title = "Alertmanager";
      link = config.services.prometheus.alertmanager.webExternalUrl;
      description = "notifies me when something goes wrong.";
    }
  ];

  services.prometheus.rules = let
    diskFreeThreshold = 10;
  in [
    (builtins.toJSON {
      groups = [
        {
          name = "niveum";
          rules = [
            {
              alert = "ServiceDown";
              expr = ''node_systemd_unit_state{state="failed"} == 1'';
              annotations = {
                summary = "{{$labels.name}} failed on {{$labels.job}}";
              };
            }
            {
              alert = "RootPartitionFull";
              for = "10m";
              expr = ''(node_filesystem_free_bytes{mountpoint="/"} * 100) / node_filesystem_size_bytes{mountpoint="/"} < ${toString diskFreeThreshold}'';
              annotations = {
                summary = ''{{ $labels.job }} running out of space: {{ $value | printf "%.2f" }}% < ${toString diskFreeThreshold}%'';
              };
            }
            {
              alert = "RootPartitionFullWeek";
              for = "1h";
              expr =
                ''node_filesystem_free_bytes{mountpoint="/"} ''
                + ''and predict_linear(node_filesystem_free_bytes{mountpoint="/"}[2d], 7*24*3600) <= 0'';
              annotations = {
                summary = "{{$labels.job}} running out of space in 7 days";
              };
            }
            {
              alert = "HighLoad";
              expr = ''node_load15 / on(job) count(node_cpu_seconds_total{mode="system"}) by (job) >= 1.0'';
              for = "10m";
              annotations = {
                summary = "{{$labels.job}} running on high load: {{$value}}";
              };
            }
            {
              alert = "HighRAM";
              expr = "node_memory_MemFree_bytes + node_memory_Buffers_bytes + node_memory_Cached_bytes < node_memory_MemTotal_bytes * 0.1";
              for = "1h";
              annotations.summary = "{{$labels.job}} using lots of RAM";
            }
            {
              alert = "UptimeMonster";
              expr = "time() - node_boot_time_seconds > 2592000";
              annotations.summary = "uptime monster {{$labels.job}} up for more than 30 days";
            }
            {
              alert = "HostDown";
              expr = ''up == 0'';
              for = "5m";
              annotations = {
                summary = "{{ $labels.job }} seeming down since 5 minutes";
              };
            }
            {
              alert = "Reboot";
              expr = "time() - node_boot_time_seconds < 300";
              annotations.summary = "{{$labels.job}} rebooted";
            }
            {
              alert = "ProbeFailed";
              expr = "probe_success == 0";
              for = "5m";
              annotations.summary = "HTTP probe failed for {{$labels.instance}}";
            }
            {
              alert = "SlowProbe";
              expr = "avg_over_time(probe_http_duration_seconds[1m]) > 1";
              for = "5m";
              annotations.summary = "HTTP probe slow for {{$labels.instance}}";
            }
            {
              alert = "HttpStatusCode";
              expr = "probe_http_status_code != 0 AND (probe_http_status_code <= 199 OR probe_http_status_code >= 400)";
              for = "5m";
              annotations.summary = "status code {{$value}} for {{$labels.instance}}";
            }
            {
              alert = "SslExpirySoon";
              expr = "probe_ssl_earliest_cert_expiry - time() < 86400 * 30";
              for = "5m";
              annotations.summary = "SSL certificate for {{$labels.instance}} expires in 30 days";
            }
            {
              alert = "SslExpiry";
              expr = "probe_ssl_earliest_cert_expiry - time()  <= 0";
              for = "5m";
              annotations.summary = "SSL certificate for {{$labels.instance}} has expired";
            }
          ];
        }
      ];
    })
  ];

  services.prometheus.alertmanager = {
    enable = true;
    listenAddress = "localhost";
    webExternalUrl = "http://alertmanager.kmein.r";
    environmentFile = config.age.secrets.alertmanager-token-reporters.path;
    configuration = {
      route = {
        group_wait = "30s";
        repeat_interval = "24h";
        receiver = "all";
      };
      receivers = [
        {
          name = "all";
          telegram_configs = [
            {
              bot_token = "$TELEGRAM_TOKEN";
              chat_id = 18980945;
              parse_mode = "";
              api_url = "https://api.telegram.org";
              send_resolved = true;
              message = ''
                {{range .Alerts -}}
                {{ .Status }}: {{ index .Annotations "summary" }}
                {{end -}}
              '';
            }
          ];
          email_configs = let
            inherit (import ../../../lib) kieran;
            inherit (import ../../../lib/email.nix {inherit lib;}) cock;
            cockConfig = {
              send_resolved = true;
              to = kieran.email;
              from = cock.user;
              smarthost = "${cock.smtp}:587";
              auth_username = cock.user;
              auth_identity = cock.user;
              auth_password = "$EMAIL_PASSWORD";
            };
          in [
          ];
        }
      ];
    };
  };

  age.secrets = {
    email-password-cock = {
      file = ../../../secrets/email-password-cock.age;
      owner = "grafana";
      group = "grafana";
      mode = "440";
    };
    grafana-password-admin = {
      file = ../../../secrets/grafana-password-admin.age;
      owner = "grafana";
      group = "grafana";
      mode = "440";
    };
    alertmanager-token-reporters = {
      file = ../../../secrets/alertmanager-token-reporters.age;
      owner = "prometheus";
      group = "prometheus";
      mode = "440";
    };
    home-assistant-token = {
      file = ../../../secrets/home-assistant-token.age;
      owner = "prometheus";
      group = "prometheus";
      mode = "440";
    };
  };

  services.prometheus.alertmanagers = [
    {
      scheme = "http";
      path_prefix = "/";
      static_configs = [{targets = ["localhost:${toString config.services.prometheus.alertmanager.port}"];}];
    }
  ];

  # otherwise bearer_token_file will fail
  services.prometheus.checkConfig = "syntax-only";

  services.prometheus.scrapeConfigs = [
    {
      job_name = "makanek";
      static_configs = [
        {
          targets = [
            "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
          ];
        }
      ];
    }
    {
      scrape_interval = "5m";
      job_name = "blackbox";
      metrics_path = "/probe";
      params.module = ["http_2xx"];
      relabel_configs = [
        {
          source_labels = ["__address__"];
          target_label = "__param_target";
        }
        {
          source_labels = ["__param_target"];
          target_label = "instance";
        }
        {
          replacement = "127.0.0.1:${toString config.services.prometheus.exporters.blackbox.port}";
          target_label = "__address__";
        }
      ];
      static_configs = [
        {
          targets = [
            "https://alew.hu-berlin.de"
            "https://beta.alew.hu-berlin.de"
            "https://alew.hu-berlin.de/api/search?substring=die&domain=lemma&derivations=true&addition=true&diacritics=false&position=infix"
            "https://beta.alew.hu-berlin.de/api/search?substring=die&domain=lemma&derivations=true&addition=true&diacritics=false&position=infix"
            "https://pad.kmein.de"
            "https://code.kmein.de"
            "https://radio.kmein.de"
            "https://tarot.kmein.de"
            "https://cloud.kmein.de"
            "http://grafana.kmein.r"
            # "names.kmein.r"
            "http://rrm.r"
            "http://graph.r"
          ];
        }
      ];
    }
    {
      job_name = "tahina";
      static_configs = [
        {
          targets = [
            "tahina.r:${toString config.services.prometheus.exporters.node.port}"
            "tahina.r:${toString restic.port}"
          ];
        }
      ];
    }
    {
      job_name = "home_assistant";
      scrape_interval = "60s";
      metrics_path = "/api/prometheus";
      scheme = "http";
      static_configs = [{targets = ["tahina.r:8123"];}];
      bearer_token_file = config.age.secrets.home-assistant-token.path;
    }
    {
      job_name = "ful";
      static_configs = [
        {
          targets = [
            "ful.r:${toString config.services.prometheus.exporters.node.port}"
          ];
        }
      ];
    }
  ];

  services.prometheus.exporters.blackbox = {
    enable = true;
    configFile = (pkgs.formats.yaml {}).generate "blackbox.yaml" blackboxConfig;
  };

  networking.firewall.allowedTCPPorts = [
    lokiConfig.server.http_listen_port
  ];

  services.loki = {
    enable = true;
    configFile = (pkgs.formats.yaml {}).generate "loki.yaml" lokiConfig;
  };
}
