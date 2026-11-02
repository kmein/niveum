# Prometheus alerting rules for the niveum fleet.
#
# every rule here emails (see ./default.nix), so a rule only belongs here if it
# means something is broken and needs a hand. annotations.description is the
# line that shows up in the mail.
let
  # real filesystems only. /nix/store is a bind mount of / on every host and would
  # otherwise duplicate every filesystem alert
  fs = ''fstype!~"tmpfs|ramfs|nsfs|squashfs|overlay|autofs|fuse.*",mountpoint!="/nix/store"'';

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
          annotations.description = "{{$labels.job}} target {{$labels.instance}} has been unreachable for 5 minutes";
        }
        {
          alert = "HostClockNotSynchronising";
          expr = "node_timex_sync_status == 0";
          for = "30m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "clock on {{$labels.job}} is not synchronised";
        }
        {
          alert = "HostClockSkew";
          expr = "abs(node_timex_offset_seconds) > 0.1";
          for = "10m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "clock on {{$labels.job}} is off by {{$value | humanizeDuration}}";
        }
        {
          alert = "HostTemperatureHigh";
          expr = "node_hwmon_temp_celsius > 85";
          for = "10m";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.job}} sensor {{$labels.chip}}/{{$labels.sensor}} at {{$value | printf "%.0f"}}°C'';
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
          # hourly timer units drop out of "failed" while they rerun; without this
          # every run is a resolve/fire mail pair
          keep_firing_for = "1h";
          labels.host = "{{ $labels.job }}";
          annotations.description = "{{$labels.name}} failed on {{$labels.job}}";
        }
        {
          # node_exporter does not export per-unit state for mounts, scopes, slices
          # and devices, so a degraded system can have no failed unit in Prometheus.
          # Only fire when SystemdUnitFailed cannot already explain the degradation.
          alert = "SystemdDegraded";
          expr = ''node_systemd_system_running == 0 unless on(instance) (count by (instance) (node_systemd_unit_state{state="failed"} == 1) > 0)'';
          for = "15m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "systemd on {{$labels.job}} is degraded but exports no failed unit — check `systemctl --failed` for mounts/scopes";
        }
      ];
    }

    {
      name = "resources";
      rules = [
        {
          alert = "HostMemoryLow";
          # MemAvailable accounts for reclaimable slab and page cache, unlike free+buffers+cached
          expr = "node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100 < 10";
          for = "30m";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.job}} has {{$value | printf "%.1f"}}% memory available'';
        }
        {
          alert = "HostOomKill";
          expr = "increase(node_vmstat_oom_kill[10m]) > 0";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.job}} OOM-killed {{$value | printf "%.0f"}} process(es) in the last 10 minutes'';
        }
        {
          alert = "HostFileDescriptorsLow";
          expr = "node_filefd_allocated / node_filefd_maximum > 0.8";
          for = "10m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "{{$labels.job}} has used {{$value | humanizePercentage}} of its file descriptors";
        }
        {
          alert = "HostConntrackNearLimit";
          expr = "node_nf_conntrack_entries / node_nf_conntrack_entries_limit > 0.8";
          for = "10m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "conntrack table on {{$labels.job}} is {{$value | humanizePercentage}} full";
        }
      ];
    }

    {
      name = "storage";
      rules = [
        {
          alert = "FilesystemFull";
          # avail, not free: free includes the blocks reserved for root.
          # a read-only filesystem cannot fill up any further
          expr = "node_filesystem_avail_bytes{${fs}} / node_filesystem_size_bytes{${fs}} * 100 < 10 unless on(instance, mountpoint) node_filesystem_readonly == 1";
          for = "15m";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.job}}:{{$labels.mountpoint}} has {{$value | printf "%.1f"}}% space left'';
        }
        {
          alert = "FilesystemReadOnly";
          # zaatar mounts its old restic repository read-only on purpose (systems/zaatar/backup.nix)
          expr = ''node_filesystem_readonly{${fs},mountpoint!="/backup"} == 1'';
          for = "5m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "{{$labels.job}}:{{$labels.mountpoint}} has been remounted read-only";
        }
        {
          alert = "FilesystemInodesLow";
          expr = "node_filesystem_files_free{${fs}} / node_filesystem_files{${fs}} * 100 < 10 unless on(instance, mountpoint) node_filesystem_readonly == 1";
          for = "15m";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.job}}:{{$labels.mountpoint}} has {{$value | printf "%.1f"}}% inodes left'';
        }
      ];
    }

    {
      name = "network";
      rules = [
        {
          alert = "HostNetworkInterfaceErrors";
          expr = ''
            rate(node_network_receive_errs_total{${netdev}}[10m])
            + rate(node_network_transmit_errs_total{${netdev}}[10m]) > 0.1
          '';
          for = "15m";
          labels.host = "{{ $labels.job }}";
          annotations.description = ''{{$labels.device}} on {{$labels.job}} sees {{$value | printf "%.2f"}} errors/s'';
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
          # radio flaps for a few minutes at a time
          keep_firing_for = "30m";
          annotations.description = "probe failed for {{$labels.instance}}";
        }
        {
          # 14 days, not 30: ACME renews at 30 days left, so a 30 day alert fires on
          # every perfectly normal renewal cycle
          alert = "SslCertExpiringSoon";
          expr = "(probe_ssl_earliest_cert_expiry - time() > 0) < 14 * 86400";
          for = "1h";
          annotations.description = "certificate for {{$labels.instance}} expires in {{$value | humanizeDuration}}";
        }
        {
          alert = "SslCertExpired";
          expr = "probe_ssl_earliest_cert_expiry - time() <= 0";
          for = "5m";
          annotations.description = "certificate for {{$labels.instance}} has expired";
        }
      ];
    }

    {
      name = "services";
      rules = [
        {
          # configs/restic-client.nix records when a backup last succeeded — a timer
          # that fired says nothing about what landed. scoped to the always-on hosts:
          # laptops back up daily but are neither on daily nor scraped
          alert = "BackupStale";
          expr = "time() - restic_backup_success_timestamp_seconds > 26 * 3600";
          for = "1h";
          labels.host = "{{ $labels.job }}";
          annotations.description = "the last successful restic backup on {{$labels.job}} was {{$value | humanizeDuration}} ago";
        }
        {
          alert = "PostgresDown";
          expr = "pg_up == 0";
          for = "5m";
          annotations.description = "postgres is not answering the exporter";
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
          annotations.description = "Prometheus failed to reload its configuration";
        }
        {
          alert = "PrometheusRuleEvaluationFailures";
          expr = "increase(prometheus_rule_evaluation_failures_total[10m]) > 0";
          annotations.description = ''{{$value | printf "%.0f"}} rule evaluations failed in group {{$labels.rule_group}}'';
        }
        {
          alert = "PrometheusNotConnectedToAlertmanagers";
          expr = "prometheus_notifications_alertmanagers_discovered < 1";
          for = "10m";
          annotations.description = "Prometheus has no Alertmanager to send alerts to";
        }
        {
          alert = "PrometheusNotificationsDropped";
          expr = "increase(prometheus_notifications_dropped_total[10m]) > 0";
          annotations.description = ''Prometheus dropped {{$value | printf "%.0f"}} alert notifications'';
        }
        {
          # a malformed .prom file makes the collector drop every textfile metric,
          # which would silently disable BackupStale above
          alert = "NodeTextfileCollectorFailed";
          expr = "node_textfile_scrape_error == 1";
          for = "15m";
          labels.host = "{{ $labels.job }}";
          annotations.description = "node_exporter cannot parse a textfile metric on {{$labels.job}}";
        }
        {
          alert = "AlertmanagerConfigReloadFailed";
          expr = "alertmanager_config_last_reload_successful == 0";
          for = "5m";
          annotations.description = "Alertmanager failed to reload its configuration";
        }
        {
          # catches a failing smtp login or relay; it is only mailed once mail works
          # again, until then it shows on alertmanager.kmein.r
          alert = "AlertmanagerNotificationsFailed";
          expr = "increase(alertmanager_notifications_failed_total[10m]) > 0";
          annotations.description = ''{{$value | printf "%.0f"}} notifications to {{$labels.integration}} failed — alerts may not be reaching your inbox'';
        }
      ];
    }
  ];
}
