# Prometheus alerting rules for the niveum fleet.
#
# Conventions:
#   * one group per concern, so related alerts are evaluated (and read) together
#   * every alert carries a severity, which matrix-alertmanager turns into a colour:
#     "warning" -> orange, "none" -> blue, everything else (we use "critical") -> red
#   * annotations.description is the text that ends up in the Matrix message;
#     the alertname and instance are prepended by matrix-alertmanager already
let
  # real filesystems only. /nix/store is a bind mount of / on every host and would
  # otherwise duplicate every filesystem alert
  fs = ''fstype!~"tmpfs|ramfs|nsfs|squashfs|overlay|autofs|fuse.*",mountpoint!="/nix/store"'';

  # physical block devices, no loop/dm/sr
  disk = ''device=~"(sd|vd|nvme|xvd|hd)[a-z0-9]+"'';

  # physical interfaces only: traffic on tinc/hyprspace is also counted on the
  # underlying NIC, and container/loopback devices are not interesting
  netdev = ''device!~"lo|veth.*|podman.*|docker.*|br-.*|virbr.*|tinc.*|hyprspace"'';
in
{
  groups = [
    {
      name = "host";
      rules = [
        {
          alert = "TargetDown";
          expr = "up == 0";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "{{$labels.job}} target {{$labels.instance}} has been unreachable for 5 minutes";
        }
        {
          alert = "HostRebooted";
          expr = "time() - node_boot_time_seconds < 300";
          labels.severity = "none";
          annotations.description = "{{$labels.instance}} rebooted {{$value | humanizeDuration}} ago";
        }
        {
          alert = "UptimeMonster";
          expr = "time() - node_boot_time_seconds > 30 * 86400";
          labels.severity = "none";
          annotations.description = "uptime monster: {{$labels.instance}} up for {{$value | humanizeDuration}}";
        }
        {
          alert = "HostClockNotSynchronising";
          expr = "node_timex_sync_status == 0";
          for = "30m";
          labels.severity = "warning";
          annotations.description = "clock on {{$labels.instance}} is not synchronised";
        }
        {
          alert = "HostClockSkew";
          expr = "abs(node_timex_offset_seconds) > 0.1";
          for = "10m";
          labels.severity = "warning";
          annotations.description = "clock on {{$labels.instance}} is off by {{$value | humanizeDuration}}";
        }
        {
          alert = "HostTemperatureHigh";
          expr = "node_hwmon_temp_celsius > 85";
          for = "10m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} sensor {{$labels.chip}}/{{$labels.sensor}} at {{$value | printf "%.0f"}}°C'';
        }
      ];
    }

    {
      name = "systemd";
      rules = [
        {
          alert = "SystemdUnitFailed";
          # 5m of grace so units that fail and get restarted right away stay quiet
          expr = ''node_systemd_unit_state{state="failed"} == 1'';
          for = "5m";
          labels.severity = "warning";
          annotations.description = "{{$labels.name}} failed on {{$labels.instance}}";
        }
        {
          # node_exporter does not export per-unit state for mounts, scopes, slices
          # and devices, so a degraded system can have no failed unit in Prometheus.
          # Only fire when SystemdUnitFailed cannot already explain the degradation.
          alert = "SystemdDegraded";
          expr = ''node_systemd_system_running == 0 unless on(instance) (count by (instance) (node_systemd_unit_state{state="failed"} == 1) > 0)'';
          for = "15m";
          labels.severity = "warning";
          annotations.description = "systemd on {{$labels.instance}} is degraded but exports no failed unit — check `systemctl --failed` for mounts/scopes";
        }
      ];
    }

    {
      name = "resources";
      rules = [
        {
          alert = "HostLoadHigh";
          # per instance, not per job: a job can hold several targets
          expr = ''node_load15 / on(instance) group_left count by (instance) (node_cpu_seconds_total{mode="idle"}) > 1.5'';
          for = "30m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} at {{$value | printf "%.2f"}}x load per core for 30 minutes'';
        }
        {
          alert = "HostMemoryLow";
          # MemAvailable accounts for reclaimable slab and page cache, unlike free+buffers+cached
          expr = "node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100 < 10";
          for = "30m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} has {{$value | printf "%.1f"}}% memory available'';
        }
        {
          alert = "HostSwapLow";
          expr = "(node_memory_SwapFree_bytes / node_memory_SwapTotal_bytes * 100 < 20) and node_memory_SwapTotal_bytes > 0";
          for = "30m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} has {{$value | printf "%.1f"}}% swap left'';
        }
        {
          alert = "HostOomKill";
          expr = "increase(node_vmstat_oom_kill[10m]) > 0";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} OOM-killed {{$value | printf "%.0f"}} process(es) in the last 10 minutes'';
        }
        {
          alert = "HostFileDescriptorsLow";
          expr = "node_filefd_allocated / node_filefd_maximum > 0.8";
          for = "10m";
          labels.severity = "warning";
          annotations.description = "{{$labels.instance}} has used {{$value | humanizePercentage}} of its file descriptors";
        }
        {
          alert = "HostConntrackNearLimit";
          expr = "node_nf_conntrack_entries / node_nf_conntrack_entries_limit > 0.8";
          for = "10m";
          labels.severity = "warning";
          annotations.description = "conntrack table on {{$labels.instance}} is {{$value | humanizePercentage}} full";
        }
      ];
    }

    {
      name = "storage";
      rules = [
        {
          alert = "FilesystemFull";
          # avail, not free: free includes the blocks reserved for root
          expr = "node_filesystem_avail_bytes{${fs}} / node_filesystem_size_bytes{${fs}} * 100 < 10";
          for = "15m";
          labels.severity = "critical";
          annotations.description = ''{{$labels.instance}}:{{$labels.mountpoint}} has {{$value | printf "%.1f"}}% space left'';
        }
        {
          alert = "FilesystemFillingUp";
          expr = ''
            (node_filesystem_avail_bytes{${fs}} / node_filesystem_size_bytes{${fs}} * 100 < 40)
            and predict_linear(node_filesystem_avail_bytes{${fs}}[24h], 7 * 24 * 3600) < 0
          '';
          for = "2h";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}}:{{$labels.mountpoint}} is at {{$value | printf "%.1f"}}% free and runs out of space within a week'';
        }
        {
          alert = "FilesystemReadOnly";
          expr = "node_filesystem_readonly{${fs}} == 1";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "{{$labels.instance}}:{{$labels.mountpoint}} has been remounted read-only";
        }
        {
          alert = "FilesystemInodesLow";
          expr = "node_filesystem_files_free{${fs}} / node_filesystem_files{${fs}} * 100 < 10";
          for = "15m";
          labels.severity = "critical";
          annotations.description = ''{{$labels.instance}}:{{$labels.mountpoint}} has {{$value | printf "%.1f"}}% inodes left'';
        }
        {
          alert = "FilesystemInodesFillingUp";
          expr = ''
            (node_filesystem_files_free{${fs}} / node_filesystem_files{${fs}} * 100 < 20)
            and predict_linear(node_filesystem_files_free{${fs}}[6h], 24 * 3600) < 0
          '';
          for = "1h";
          labels.severity = "warning";
          annotations.description = "{{$labels.instance}}:{{$labels.mountpoint}} runs out of inodes within a day";
        }
        {
          # replaces the old read/write throughput alerts: absolute MB/s says nothing
          # about whether the disk is actually the bottleneck, utilisation does
          alert = "DiskIOSaturated";
          expr = "rate(node_disk_io_time_seconds_total{${disk}}[15m]) > 0.9";
          for = "30m";
          labels.severity = "warning";
          annotations.description = "{{$labels.device}} on {{$labels.instance}} was busy {{$value | humanizePercentage}} of the time for 30 minutes";
        }
      ];
    }

    {
      name = "network";
      rules = [
        {
          alert = "HostNetworkThroughputIn";
          expr = "rate(node_network_receive_bytes_total{${netdev}}[5m]) > 100 * 1024 * 1024";
          for = "15m";
          labels.severity = "warning";
          annotations.description = "{{$labels.instance}} is receiving {{$value | humanize}}B/s on {{$labels.device}}";
        }
        {
          alert = "HostNetworkThroughputOut";
          expr = "rate(node_network_transmit_bytes_total{${netdev}}[5m]) > 100 * 1024 * 1024";
          for = "15m";
          labels.severity = "warning";
          annotations.description = "{{$labels.instance}} is sending {{$value | humanize}}B/s on {{$labels.device}}";
        }
        {
          alert = "HostNetworkInterfaceErrors";
          expr = ''
            rate(node_network_receive_errs_total{${netdev}}[10m])
            + rate(node_network_transmit_errs_total{${netdev}}[10m]) > 0.1
          '';
          for = "15m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.device}} on {{$labels.instance}} sees {{$value | printf "%.2f"}} errors/s'';
        }
      ];
    }

    {
      name = "blackbox";
      rules = [
        {
          alert = "ProbeFailed";
          expr = "probe_success == 0";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "probe failed for {{$labels.instance}}";
        }
        {
          # routed to the krebs room, hence a separate alert from ProbeFailed
          alert = "Mastodon";
          expr = ''probe_success{instance="https://social.krebsco.de"} == 0'';
          for = "5m";
          labels.severity = "critical";
          annotations.description = "Mastodon instance {{$labels.instance}} is down";
        }
        {
          # probe_duration_seconds is the whole probe; probe_http_duration_seconds is
          # split per phase and would compare each phase against the threshold
          alert = "ProbeSlow";
          expr = "avg_over_time(probe_duration_seconds[15m]) > 2";
          for = "15m";
          labels.severity = "warning";
          annotations.description = ''{{$labels.instance}} takes {{$value | printf "%.1f"}}s to respond'';
        }
        {
          alert = "ProbeHttpError";
          expr = "probe_http_status_code >= 400";
          for = "10m";
          labels.severity = "warning";
          annotations.description = "{{$labels.instance}} answers with HTTP {{$value}}";
        }
        {
          # 14 days, not 30: ACME renews at 30 days left, so a 30 day alert fires on
          # every perfectly normal renewal cycle
          alert = "SslCertExpiringSoon";
          expr = "(probe_ssl_earliest_cert_expiry - time() > 0) < 14 * 86400";
          for = "1h";
          labels.severity = "warning";
          annotations.description = "certificate for {{$labels.instance}} expires in {{$value | humanizeDuration}}";
        }
        {
          alert = "SslCertExpired";
          expr = "probe_ssl_earliest_cert_expiry - time() <= 0";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "certificate for {{$labels.instance}} has expired";
        }
      ];
    }

    {
      name = "services";
      rules = [
        {
          # timers are Persistent, so the last trigger survives reboots.
          # "> 0" skips timers that have never run.
          alert = "BackupStale";
          expr = ''node_systemd_timer_last_trigger_seconds{name=~"restic-backups-.*"} > 0 < time() - 26 * 3600'';
          for = "1h";
          labels.severity = "critical";
          annotations.description = "{{$labels.name}} on {{$labels.instance}} last ran {{$value | humanizeTimestamp}}";
        }
        {
          alert = "OffsiteBackupStale";
          expr = ''node_systemd_timer_last_trigger_seconds{name="restic-rsync-offsite.timer"} > 0 < time() - 8 * 86400'';
          for = "1h";
          labels.severity = "warning";
          annotations.description = "offsite mirror on {{$labels.instance}} last ran {{$value | humanizeTimestamp}}";
        }
        {
          alert = "RedisDown";
          expr = "redis_up == 0";
          for = "10m";
          labels.severity = "critical";
          annotations.description = "redis on {{$labels.instance}} is not answering the exporter";
        }
        {
          alert = "HomeAssistantBatteryLow";
          expr = "homeassistant_sensor_battery_percent < 20";
          for = "6h";
          labels.severity = "none";
          annotations.description = ''{{$labels.friendly_name}} battery at {{$value | printf "%.0f"}}%'';
        }
      ];
    }

    {
      # monitoring the monitoring: without these, a broken alert path is silent
      name = "monitoring";
      rules = [
        {
          alert = "PrometheusConfigReloadFailed";
          expr = "prometheus_config_last_reload_successful == 0";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "Prometheus failed to reload its configuration";
        }
        {
          alert = "PrometheusRuleEvaluationFailures";
          expr = "increase(prometheus_rule_evaluation_failures_total[10m]) > 0";
          labels.severity = "warning";
          annotations.description = ''{{$value | printf "%.0f"}} rule evaluations failed in group {{$labels.rule_group}}'';
        }
        {
          alert = "PrometheusNotConnectedToAlertmanagers";
          expr = "prometheus_notifications_alertmanagers_discovered < 1";
          for = "10m";
          labels.severity = "critical";
          annotations.description = "Prometheus has no Alertmanager to send alerts to";
        }
        {
          alert = "PrometheusNotificationsDropped";
          expr = "increase(prometheus_notifications_dropped_total[10m]) > 0";
          labels.severity = "critical";
          annotations.description = ''Prometheus dropped {{$value | printf "%.0f"}} alert notifications'';
        }
        {
          alert = "AlertmanagerConfigReloadFailed";
          expr = "alertmanager_config_last_reload_successful == 0";
          for = "5m";
          labels.severity = "critical";
          annotations.description = "Alertmanager failed to reload its configuration";
        }
        {
          # this is what catches a broken matrix-alertmanager webhook
          alert = "AlertmanagerNotificationsFailed";
          expr = "increase(alertmanager_notifications_failed_total[10m]) > 0";
          labels.severity = "critical";
          annotations.description = ''{{$value | printf "%.0f"}} notifications to {{$labels.integration}} failed — alerts may not be reaching Matrix'';
        }
      ];
    }
  ];
}
