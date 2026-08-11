{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (pkgs.lib.niveum) domain;
  lokiConfig = import ./loki.nix;
  blackboxConfig = import ./blackbox.nix;

  # every blackbox job scrapes the local exporter and passes the real target as a
  # parameter, so only the module and the target list differ between them
  blackboxJob = job_name: module: targets: {
    # 1m, so that a 5m "for" on a probe alert means five samples and not one
    scrape_interval = "1m";
    inherit job_name;
    metrics_path = "/probe";
    params.module = [ module ];
    relabel_configs = [
      {
        source_labels = [ "__address__" ];
        target_label = "__param_target";
      }
      {
        source_labels = [ "__param_target" ];
        target_label = "instance";
      }
      {
        replacement = "127.0.0.1:${toString config.services.prometheus.exporters.blackbox.port}";
        target_label = "__address__";
      }
    ];
    static_configs = [ { inherit targets; } ];
  };

  # only the always-on hosts: ProbeFailed is critical and has no job selector, so
  # probing laptops would page on every closed lid
  probedHosts = [
    "makanek"
    "zaatar"
    "ful"
  ];
in
{
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
        secret_key = "SW2YcwTIb9zpOOhoPsMm";
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

  services.prometheus.rules = [ (builtins.toJSON (import ./rules.nix)) ];

  # forwards alertmanager webhooks to matrix, one room per receiver
  services.matrix-alertmanager = {
    enable = true;
    port = 9088;
    homeserverUrl = "https://matrix.4d2.org";
    matrixUser = "@lakai:4d2.org";
    matrixRooms = [
      {
        receivers = [ "matrix" ];
        roomId = "!zlwCuPiCNMSxDviFzA:4d2.org";
      }
      {
        receivers = [ "lassulus" ];
        roomId = "!MJAGqBAOKZGMywzwkI:lassul.us";
      }
    ];
    tokenFile = config.age.secrets.matrix-token-lakai.path;
    secretFile = config.age.secrets.matrix-alertmanager-secret.path;
  };

  services.prometheus.alertmanager = {
    enable = true;
    listenAddress = "localhost";
    webExternalUrl = "http://alertmanager.kmein.r";
    environmentFile = config.age.secrets.alertmanager-token-reporters.path;
    configuration = {
      route = {
        group_by = [
          "alertname"
          "severity"
        ];
        group_wait = "30s";
        repeat_interval = "24h";
        receiver = "matrix";
        routes = [
          {
            receiver = "lassulus";
            matchers = [ "alertname = \"Mastodon\"" ];
          }
          {
            # informational alerts (uptime monster, reboots, low batteries) are true
            # for days on end; do not repeat them daily
            receiver = "matrix";
            matchers = [ "severity = \"none\"" ];
            repeat_interval = "168h";
          }
        ];
      };
      receivers =
        let
          # $MATRIX_ALERTMANAGER_SECRET comes from environmentFile via envsubst
          webhook = {
            url = "http://localhost:${toString config.services.matrix-alertmanager.port}/alerts?secret=$MATRIX_ALERTMANAGER_SECRET";
            max_alerts = 5;
          };
        in
        [
          {
            name = "lassulus";
            webhook_configs = [ webhook ];
          }
          {
            name = "matrix";
            webhook_configs = [ webhook ];
          }
        ];
    };
  };

  age.secrets = {
    matrix-token-lakai.file = ../../../secrets/matrix-token-lakai.age;
    matrix-alertmanager-secret.file = ../../../secrets/matrix-alertmanager-secret.age;
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
      static_configs = [
        { targets = [ "localhost:${toString config.services.prometheus.alertmanager.port}" ]; }
      ];
    }
  ];

  # otherwise bearer_token_file will fail
  services.prometheus.checkConfig = "syntax-only";

  services.prometheus.extraFlags = [
    "--storage.tsdb.retention.time=7d"
    "--storage.tsdb.retention.size=2GB"
    "--storage.tsdb.wal-compression"
  ];

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
      # the monitoring group of ./rules.nix and the prometheus dashboard need this
      job_name = "prometheus";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.port}" ]; }
      ];
    }
    {
      job_name = "alertmanager";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.alertmanager.port}" ]; }
      ];
    }
    {
      job_name = "loki";
      static_configs = [
        { targets = [ "127.0.0.1:${toString lokiConfig.server.http_listen_port}" ]; }
      ];
    }
    {
      job_name = "nginx";
      static_configs = [
        {
          targets = [
            "127.0.0.1:${toString config.services.prometheus.exporters.nginx.port}"
            "ful.r:${toString config.services.prometheus.exporters.nginx.port}"
          ];
        }
      ];
    }
    {
      job_name = "php_fpm";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.php-fpm.port}" ]; }
      ];
    }
    {
      job_name = "postgres";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.postgres.port}" ]; }
      ];
    }
    {
      # every scrape logs into nextcloud and walks the serverinfo API
      job_name = "nextcloud";
      scrape_interval = "5m";
      scrape_timeout = "30s";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.nextcloud.port}" ]; }
      ];
    }
    {
      # the repository, and therefore the exporter, lives on zaatar
      job_name = "restic";
      scrape_interval = "5m";
      scrape_timeout = "1m";
      static_configs = [
        { targets = [ "zaatar.r:${toString config.services.prometheus.exporters.restic.port}" ]; }
      ];
    }
    (blackboxJob "blackbox" "http_2xx" [
      "https://pad.${domain}"
      "https://code.${domain}"
      "https://radio.${domain}"
      "https://tarot.${domain}"
      "https://iching.${domain}"
      "https://social.krebsco.de"
      "https://cloud.${domain}"
      "http://grafana.kmein.r"
      # "names.kmein.r"
      "http://rrm.r"
      "http://graph.r"
    ])
    (blackboxJob "icmp" "icmp" (map (host: "${host}.r") probedHosts))
    (blackboxJob "ssh" "tcp_connect" (
      map (host: "${host}.r:${toString pkgs.lib.niveum.sshPort}") probedHosts
    ))
    {
      job_name = "zaatar";
      static_configs = [
        {
          targets = [
            "zaatar.r:${toString config.services.prometheus.exporters.node.port}"
            "zaatar.r:${toString pkgs.lib.niveum.restic.port}"
          ];
        }
      ];
    }
    {
      job_name = "brockman";
      static_configs = [
        {
          targets = [
            "brockman.news:9002" # node
            "brockman.news:9121" # redis
            "brockman.news:9113" # nginx
          ];
        }
      ];
    }
    {
      job_name = "home_assistant";
      scrape_interval = "60s";
      metrics_path = "/api/prometheus";
      scheme = "http";
      static_configs = [ { targets = [ "zaatar.r:8123" ]; } ];
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
    configFile = (pkgs.formats.yaml { }).generate "blackbox.yaml" blackboxConfig;
  };

  networking.firewall.allowedTCPPorts = [
    lokiConfig.server.http_listen_port
  ];

  services.loki = {
    enable = true;
    configFile = (pkgs.formats.yaml { }).generate "loki.yaml" lokiConfig;
  };
}
