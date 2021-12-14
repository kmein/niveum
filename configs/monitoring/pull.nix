{ lib, config, pkgs, ... }:
let
  lokiConfig = import ./loki.nix;
  blackboxConfig = import ./blackbox.nix;
in
{
  services.grafana = {
    enable = true;
    domain = "grafana.kmein.r";
    port = 9444;
    addr = "127.0.0.1";
  };

  services.nginx.virtualHosts.${config.services.grafana.domain} = {
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
          expr = ''node_systemd_unit_state{state="failed"} == 1'';
          annotations = {
            summary = "{{$labels.job}}: Service {{$labels.name}} failed to start.";
          };
        }
        {
          alert = "RootPartitionFull";
          for = "10m";
          expr = ''(node_filesystem_free_bytes{mountpoint="/"} * 100) / node_filesystem_size_bytes{mountpoint="/"} < ${toString diskFreeThreshold}'';
          annotations = {
            summary = "{{ $labels.job }}: Filesystem is running out of space soon.";
            description = ''The root disk of {{ $labels.job }} has {{ $value | printf "%.2f" }}% free disk space (threshold at ${toString diskFreeThreshold}%).'';
          };
        }
        {
          alert = "RootPartitionFullWeek";
          for = "1h";
          expr = ''node_filesystem_free_bytes{mountpoint="/"} ''
            + ''and predict_linear(node_filesystem_free_bytes{mountpoint="/"}[2d], 7*24*3600) <= 0'';
          annotations = {
            summary = "{{$labels.job}}: Filesystem is running out of space in 7 days.";
          };
        }
        {
          alert = "HighLoad";
          expr = ''node_load15 / on(job) count(node_cpu_seconds_total{mode="system"}) by (job) >= 1.0'';
          for = "10m";
          annotations = {
            summary = "{{$labels.job}}: Running on high load: {{$value}}";
          };
        }
        {
          alert = "HighRAM";
          expr = "node_memory_MemFree_bytes + node_memory_Buffers_bytes + node_memory_Cached_bytes < node_memory_MemTotal_bytes * 0.1";
          for = "1h";
          annotations.summary = "{{$labels.job}}: Using lots of RAM.";
        }
        {
          alert = "UptimeMonster";
          expr = "time() - node_boot_time_seconds > 2592000";
          annotations.summary = "{{$labels.job}}: up for more than 30 days.";
        }
        {
          alert = "HostDown";
          expr = ''up == 0'';
          for = "5m";
          annotations = {
            summary = "Host {{ $labels.job }} down for 5 minutes.";
          };
        }
        {
          alert = "Reboot";
          expr = "time() - node_boot_time_seconds < 300";
          annotations.summary = "{{$labels.job}}: Reboot";
        }
        {
          alert = "ProbeFailed";
          expr = "probe_success == 0";
          for = "5m";
          annotations.summary = "{{$labels.instance}}: probe failed";
        }
        {
          alert = "SlowProbe";
          expr = "avg_over_time(probe_http_duration_seconds[1m]) > 1";
          for = "5m";
          annotations.summary = "{{$labels.instance}}: HTTP probe slow";
        }
        {
          alert = "HttpStatusCode";
          expr = "probe_http_status_code != 0 AND (probe_http_status_code <= 199 OR probe_http_status_code >= 400)";
          for = "5m";
          annotations.summary = "{{$labels.instance}}: status code {{$value}}";
        }
        {
          alert = "SslExpirySoon";
          expr = "probe_ssl_earliest_cert_expiry - time() < 86400 * 30";
          for = "5m";
          annotations.summary = "{{$labels.instance}}: SSL certificate expires in 30 days";
        }
        {
          alert = "SslExpiry";
          expr = "probe_ssl_earliest_cert_expiry - time()  <= 0";
          for = "5m";
          annotations.summary = "{{$labels.instance}}: SSL certificate has expired";
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
        --template.paths=${pkgs.writeText "template.tmpl" ''
          {{ define "telegram.default" }}
          {{range .Alerts -}}
          {{ if eq .Status "firing" }}
          ⚠ <b>{{ index .Annotations "summary"}}</b>
          {{ index .Annotations "description" }}

          See on Grafana: http://${config.services.grafana.domain}/d/alpUteInz/niveum
          {{ else -}}
          RESOLVED 😌 <del>{{ index .Annotations "summary"}}</del>
          {{- end }}
          {{end -}}
          {{end}}
        ''}'';
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
      ]; } ];
    }
    {
      job_name = "blackbox";
      metrics_path = "/probe";
      params.module = [ "http_2xx" ];
      relabel_configs = [
        { source_labels = ["__address__"]; target_label = "__param_target"; }
        { source_labels = ["__param_target"]; target_label = "instance"; }
        { replacement = "127.0.0.1:${toString config.services.prometheus.exporters.blackbox.port}"; target_label = "__address__"; }
      ];
      static_configs = [{
        targets = [
          "alew.hu-berlin.de"
        ];
      }];
    }
    {
      job_name = "zaatar";
      static_configs = [ { targets = [ "zaatar.r:${toString config.services.prometheus.exporters.node.port}" ]; } ];
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
