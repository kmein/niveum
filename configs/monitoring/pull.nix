{ lib, config, pkgs, ... }:
let
  lokiConfig = import ./loki.nix;
in
{
  services.grafana = {
    enable = true;
    domain = "monitoring.xn--kiern-0qa.de";
    port = 2342;
    addr = "127.0.0.1";
  };

  services.nginx.virtualHosts.${config.services.grafana.domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
      proxyWebsockets = true;
    };
  };

  services.prometheus.rules = let diskFreeThreshold = 10; in [(builtins.toJSON {
    groups = [{
      name = "niveum";
      rules = [
        {
          alert = "ServiceDown";
          for = "5m";
          expr = ''node_systemd_unit_state{state="failed"} == 1'';
          labels.severity = "warning";
          annotations = {
            summary = "{{ $labels.name }} is down.";
          };
        }
        {
          alert = "RootPartitionFull";
          for = "30m";
          expr = ''(node_filesystem_avail_bytes{mountpoint="/"} * 100) / node_filesystem_size_bytes{mountpoint="/"} < ${toString diskFreeThreshold}'';
          labels.severity = "warning";
          annotations = {
            summary = "{{ $labels.job }} root disk full.";
            description = ''The root disk of {{ $labels.job }} has {{ $value | printf "%.2f" }}% free disk space (threshold at ${toString diskFreeThreshold}%).'';
          };
        }
        {
          alert = "HostDown";
          expr = ''up == 0'';
          for = "5m";
          labels.severity = "warning";
          annotations = {
            summary = "Host {{ $labels.job }} down for 5 minutes.";
          };
        }
      ];
    }];
  })];

  systemd.services.alertmanager-bot-telegram =
  let
    alertmanager-bot-telegram = pkgs.buildGoModule rec {
      pname = "alertmanager-bot";
      version = "2020-07-13";
      src = pkgs.fetchFromGitHub {
        owner = "metalmatze";
        repo = "alertmanager-bot";
        rev = "5efc0bbbf8023d4324e9da98562f064a714a7206";
        sha256 = "09cciml1j8x76jpm2v5v6h2q6j1fkhsz1kswslmx8wl4wk40xgp4";
      };
      vendorSha256 = "1v0fgin8dn81b559zz4lqmrl7hikr46g4gb18sci4riql5qs1isj";
      postInstall = ''
        install -D ./default.tmpl $out/templates/default.tmpl
      '';
    };
  in {
    wantedBy = [ "multi-user.target" ];
    after = [ "ip-up.target" ];
    environment.TELEGRAM_ADMIN = "18980945";
    environment.TELEGRAM_TOKEN = lib.strings.fileContents <system-secrets/telegram/prometheus.token>;
    serviceConfig = {
      DynamicUser = true;
      StateDirectory = "alertbot";
      ExecStart = ''${alertmanager-bot-telegram}/bin/alertmanager-bot \
        --alertmanager.url=http://localhost:9093 --log.level=info \
        --store=bolt --bolt.path=/var/lib/alertbot/bot.db \
        --listen.addr="0.0.0.0:16320" \
        --template.paths=${./template.tmpl}'';
    };
  };

  services.prometheus.alertmanager = {
    enable = true;
    listenAddress = "localhost";
    configuration = {
      route = {
        group_wait = "30s";
        repeat_interval = "4h";
        receiver = "me";
      };
      receivers = [{
        name = "me";
        webhook_configs = [{
          url = "http://localhost:16320";
          send_resolved = true;
        }];
      }];
    };
  };

  services.prometheus.alertmanagers = [{
    scheme = "http";
    path_prefix = "/";
    static_configs = [ { targets = [ "localhost:9093" ]; } ];
  }];

  services.prometheus.scrapeConfigs = [
    {
      job_name = "makanek";
      static_configs = [ { targets = [
        "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
        # "127.0.0.1:${toString config.services.prometheus.exporters.nginx.port}"
      ]; } ];
    }
    {
      job_name = "zaatar";
      static_configs = [ { targets = [ "zaatar.r:${toString config.services.prometheus.exporters.node.port}" ]; } ];
    }
  ];

  networking.firewall.allowedTCPPorts = [ lokiConfig.server.http_listen_port ];

  services.loki = {
    enable = true;
    configFile = (pkgs.formats.yaml {}).generate "loki.yaml" lokiConfig;
  };
}
