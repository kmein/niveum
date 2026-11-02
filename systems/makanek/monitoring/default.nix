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
  ownOnly = pkgs.lib.niveum.nginxAllow pkgs.lib.niveum.ownAddresses;

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

  services.nginx.virtualHosts = lib.mapAttrs (_: vhost: vhost // { extraConfig = ownOnly; }) {
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

  services.prometheus.enable = true;
  # grafana-datasources.json points at this port
  services.prometheus.port = 9001;
  services.prometheus.webExternalUrl = "http://prometheus.kmein.r";

  services.prometheus.rules = [ (builtins.toJSON (import ./rules.nix)) ];

  services.prometheus.alertmanager = {
    enable = true;
    listenAddress = "localhost";
    webExternalUrl = "http://alertmanager.kmein.r";
    configuration = {
      route = {
        receiver = "email";
        # one mail per alert type, listing every host it fires on
        group_by = [ "alertname" ];
        group_wait = "30s";
        # batches further hosts joining a group and their resolutions into one mail
        group_interval = "1h";
        repeat_interval = "24h";
      };
      receivers = [
        {
          name = "email";
          email_configs = [
            {
              to = "2210@cock.li";
              from = "2210@cock.li";
              smarthost = "mail.cock.li:587";
              auth_username = "2210@cock.li";
              auth_password_file = "/run/credentials/alertmanager.service/smtp-password";
              send_resolved = true;
            }
          ];
        }
      ];
    };
  };

  # alertmanager runs as a DynamicUser and cannot read the agenix file itself
  systemd.services.alertmanager.serviceConfig.LoadCredential = [
    "smtp-password:${config.age.secrets.email-password-cock.path}"
  ];

  age.secrets = {
    email-password-cock.file = ../../../secrets/email-password-cock.age;
    grafana-password-admin = {
      file = ../../../secrets/grafana-password-admin.age;
      owner = "grafana";
      group = "grafana";
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
      job_name = "postgres";
      static_configs = [
        { targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.postgres.port}" ]; }
      ];
    }
    (blackboxJob "blackbox" "http_2xx" [
      "https://pad.${domain}"
      "https://code.${domain}"
      "https://radio.${domain}"
      "https://tarot.${domain}"
      "https://iching.${domain}"
      "http://grafana.kmein.r"
      "http://names.kmein.r"
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
          ];
        }
      ];
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
