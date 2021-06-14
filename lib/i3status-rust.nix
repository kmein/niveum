{ pkgs, wirelessInterface, colours, batteryName }:
let
  inherit (pkgs) lib;

  setsid = script:
    pkgs.writers.writeDash "setsid-command" ''
      ${pkgs.utillinux}/bin/setsid ${script}
    '';
  coronaBlock = {
    block = "custom";
    interval = 60 * 2; # every two minutes
    command = pkgs.writers.writeDash "corona" ''
      ${pkgs.curl}/bin/curl https://corona-stats.online/germany \
        | ${pkgs.gnugrep}/bin/grep Germany \
        | ${pkgs.gnused}/bin/sed 's/\s*//g' \
        | ${pkgs.ansifilter}/bin/ansifilter \
        | ${pkgs.gawk}/bin/awk -F'│' '{print "🤒 " $8 " 💀 " $5}'
    '';
  };
in {
  theme = {
    name = "plain";
    overrides = {
      critical_fg = colours.red.bright;
      good_fg = colours.green.bright;
      idle_fg = colours.foreground;
      info_fg = colours.foreground;
      warning_fg = colours.yellow.bright;
      warning_bg = colours.background;
      alternating_tint_bg = colours.background;
      alternating_tint_fg = colours.background;
      critical_bg = colours.background;
      good_bg = colours.background;
      idle_bg = colours.background;
      info_bg = colours.background;
      separator = "/ ";
      separator_bg = "auto";
      separator_fg = colours.black.bright;
    };
  };
  icons = {
    name = "none";
    overrides = {
      bat = "🔋";
      bat_charging = "🔌";
      bat_discharging = "🔋";
      bat_empty = " ";
      bat_full = " ";
      bat_half = " ";
      bat_quarter = " ";
      bat_three_quarters = " ";
      cogs = "🚦 ";
      cpu = "🖥 ";
      disk_drive = "💽";
      mail = "📧 ";
      memory_mem = "🧠 ";
      music = "🎵";
      music_next = "⏭";
      music_pause = "";
      music_play = "▶";
      music_prev = "⏮";
      net_down = "❎";
      net_up = "🌐";
      net_vpn = "🛡 ";
      net_wired = "🌐";
      net_wireless = "📶";
      pomodoro = "🍅 ";
      time = "📅 ";
      toggle_off = "👎";
      toggle_on = "👍";
      volume_empty = "🔈 ";
      volume_full = "🔊 ";
      volume_half = "🔉 ";
      volume_muted = "🔇";
      weather_clouds = "🌥";
      weather_default = "🌡";
      weather_rain = "🌧";
      weather_snow = "🌨";
      weather_sun = "🌣";
      weather_thunder = "🌩";
    };
  };
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
      interval = 60 * 60;
      command = pkgs.writers.writeDash "vax" ''
        ${pkgs.curl}/bin/curl -sSL https://api.corona-zahlen.org/vaccinations \
          | ${pkgs.jq}/bin/jq -r '"💉 Ⅰ \(.data.quote * 1000 | floor | . / 10)% Ⅱ \(.data.secondVaccination.quote * 1000 | floor | . / 10)%"'
      '';
    }
    {
      block = "custom";
      interval = 30;
      command =
        let query = "tag:unread AND tag:inbox";
        in pkgs.writers.writeDash "count-new-mail" ''
          mail_count="$(${pkgs.notmuch}/bin/notmuch search ${lib.escapeShellArg query} | wc -l)"
          [ "$mail_count" = 0 ] && printf "📭" || printf "📬"
          echo "$mail_count"
        '';
    }
    (let service = "openvpn-hu-berlin"; in {
      block = "custom";
      interval = 5;
      command = pkgs.writers.writeDash "net-device" ''
        PATH=${lib.makeBinPath [ pkgs.systemd ]}
        systemctl is-active --quiet ${service}.service && echo "🎓👍" || echo "🎓👎"
      '';
      on_click = pkgs.writers.writeDash "toggle" ''
        PATH=${lib.makeBinPath [ pkgs.systemd pkgs.libnotify ]}
        systemctl is-active --quiet ${service}.service && {
          systemctl stop ${service}.service && notify-send -a "${service}" stopped
        } || {
          systemctl start ${service}.service && notify-send -a "${service}" started
        }
      '';
    })
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
    { block = "load"; }
    {
      block = "time";
      interval = 1;
      format = "%Y-%m-%d (%W %a) %H:%M";
    }
  ];
}
