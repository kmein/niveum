{
  pkgs,
  wirelessInterface,
  colours,
  batteryName,
}: let
  inherit (pkgs) lib;

  setsid = script:
    pkgs.writers.writeDash "setsid-command" ''
      ${pkgs.util-linux}/bin/setsid ${script}
    '';

  accounts = import <niveum/lib/email.nix> {inherit lib;};
in {
  theme = {
    name = "plain";
    overrides = {
      critical_fg = colours.red.bright;
      good_fg = colours.green.bright;
      idle_fg = colours.foreground;
      info_fg = colours.cyan.bright;
      warning_fg = colours.yellow.bright;
      warning_bg = colours.background;
      alternating_tint_bg = colours.background;
      alternating_tint_fg = colours.foreground;
      critical_bg = colours.background;
      good_bg = colours.background;
      idle_bg = colours.background;
      info_bg = colours.background;
      separator = "";
      separator_bg = "auto";
      separator_fg = colours.black.bright;
    };
  };
  icons.name = "awesome6";
  icons.overrides.rss = "";
  icons.overrides.vpn = "";
  icons.overrides.irc = "";
  block = [
    {
      block = "weather";
      autolocate = true;
      format = "{location}: {temp}C";
      service = {
        name = "openweathermap";
        api_key = lib.strings.fileContents <secrets/openweathermap.key>;
        city_id = "2950159";
        units = "metric";
      };
    }
    {
      block = "custom";
      interval = 60 * 5;
      command = let
        spacetime = import <niveum/configs/spacetime.nix>;
      in
        pkgs.writers.writePython3 "sun.py" {
          libraries = [pkgs.python3Packages.astral];
          flakeIgnore = ["E121" "E501"];
        }
        ''
          import astral
          import astral.moon
          import astral.sun

          current_phase = astral.moon.phase()

          city = astral.LocationInfo("Berlin", "Germany", "${spacetime.time.timeZone}", ${toString spacetime.location.latitude}, ${toString spacetime.location.longitude})
          sun = astral.sun.sun(city.observer, date=astral.today(), tzinfo=city.timezone)

          print("↑{} ↓{} {}{}".format(sun["sunrise"].strftime("%R"), sun["sunset"].strftime("%R"), "☽" if current_phase < 14 else "☾", round(current_phase, 1)))
        '';
    }
    {
      block = "github";
      info = ["total"];
      warning = ["mention" "review_requested" "team_mention" "manual" "invitation" "assign" "subscribed"];
    }
    {
      block = "custom";
      interval = 10;
      command = "newsboat-unread-count";
      json = true;
    }
    {
      block = "custom";
      interval = 10;
      command = pkgs.writers.writeDash "todo" ''
        ${pkgs.todoman}/bin/todo --porcelain | ${pkgs.jq}/bin/jq -r '
          map(select(.due != null))
          | (map(select(.due < now)) | length) as $overdue
          | (map(select(.due >= now and .due < now + (60 * 60 * 24))) | length) as $dueToday
          | {
            icon: "tasks",
            text: (($overdue + $dueToday) as $sum | if $sum > 0 then $sum | tostring else "" end),
            state: (
              if $overdue > 0 then
                "Critical"
              elif $dueToday > 0 then
                "Warning"
              else
                "Idle"
              end
            )
          }
        '
      '';
      json = true;
      hide_when_empty = true;
    }
    {
      block = "custom";
      interval = 60 * 5;
      command = let
        query-account = name: account: "${pkgs.writers.writeDash "query-imap-${name}" ''
          ${pkgs.coreutils}/bin/timeout 1 ${pkgs.curl}/bin/curl -sSL -u ${lib.escapeShellArg "${account.user}:${account.password}"} imaps://${account.imap} -X 'STATUS INBOX (UNSEEN)' \
            | ${pkgs.gnugrep}/bin/grep -Eo '[0-9]+' \
            | sed 's/^/{"${name}":/;s/$/}/'
        ''} &";
      in
        pkgs.writers.writeDash "unread-mail" ''
          {
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList query-account accounts)}
            wait
          } | jq -s 'if length == 0 then {text: "", icon: "mail", state: "Idle"} else
            add
            | (values | add) as $sum
            | {
              text: (if $sum > 0 then $sum | tostring else "" end),
              icon: "mail",
              state: (
                if .uni > 0 or .["work-uni"] > 0 or .posteo > 0 then
                  "Warning"
                elif $sum > 0 then
                  "Info"
                else
                  "Idle"
                end
              )
            } end'
        '';
      json = true;
      hide_when_empty = true;
    }
    {
      block = "custom";
      interval = 60;
      command = pkgs.writers.writeDash "weechat" ''
        ssh -o ConnectTimeout=1 makanek cat /var/lib/weechat/hotlist.txt | sed 's/,/\n/g' | wc -l | jq '{
          text: (if . > 0 then . | tostring else "" end),
          state: (if . > 0 then "Info" else "Idle" end),
          icon: "irc"
        }'
      '';
      json = true;
      hide_when_empty = true;
    }
    {
      block = "custom";
      interval = 5;
      command = pkgs.writers.writeDash "hu-berlin-vpn" ''
        PATH=${lib.makeBinPath [pkgs.systemd]}
        (systemctl is-active --quiet openvpn-hu-berlin.service && echo '{"state": "Good", "text": "OpenVPN", "icon": "vpn"}') \
          || (systemctl is-active --quiet hu-vpn.service && echo '{"state": "Good", "text": "PPP+SSL", "icon": "vpn"}') \
          || echo '{"state": "Idle", "icon": "vpn", "text": ""}'
      '';
      json = true;
      hide_when_empty = true;
    }
    {
      block = "net";
      device = wirelessInterface;
      format = "{ssid} {signal_strength}";
    }
    {
      block = "battery";
      device = batteryName;
    }
    {
      block = "sound";
      on_click = "pavucontrol";
    }
    {
      block = "disk_space";
      format = "{icon} {available}";
    }
    {
      block = "memory";
      display_type = "memory";
      format_mem = "{mem_used;G}";
      clickable = false;
    }
    {block = "load";}
    {
      block = "custom";
      interval = 1;
      json = true;
      command = pkgs.writers.writeDash "time" ''
        ${pkgs.jq}/bin/jq -n \
          --arg now "$(${pkgs.coreutils}/bin/date +'%Y-%m-%d (%W %a) %H:%M')" \
          --argjson nextEvent "$(
            ${pkgs.khal}/bin/khal list --format "{start}" --day-format "" $(${pkgs.coreutils}/bin/date +'%Y-%m-%d %H:%M') 2>/dev/null \
            | ${pkgs.gnugrep}/bin/grep -E '[0-9]{2}:[0-9]{2}' \
            | ${pkgs.coreutils}/bin/head -1 \
            | ${pkgs.coreutils}/bin/date --date="$(cat)" +%s
          )" \
          '{
            text: $now,
            icon: "time",
            state: (
              ($nextEvent - now) as $deltaT
              | if $deltaT < (5 * 60) then
                "Critical"
              elif $deltaT < (15 * 60) then
                "Warning"
              elif $deltaT < (60 * 60) then
                "Info"
              else
                "Idle"
              end
            )
          }'
      '';
    }
  ];
}
