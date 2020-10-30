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
      separator = "* ";
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
    };
  };
  block = [
    {
      block = "custom";
      interval = 20;
      command = pkgs.writers.writeDash "tasks" ''
        ${pkgs.todo-txt-cli}/bin/todo.sh list '(.)' \
          | tail -n 1 \
          | ${pkgs.gawk}/bin/awk '{ print "⏳ " $2 "/" $4 }'
      '';
      on_click = let sleepSeconds = 2.5;
      in pkgs.writers.writeDash "show-tasks" ''
        ${pkgs.st}/bin/st -c floating -e ${pkgs.dash}/bin/dash -c "${pkgs.todo-txt-cli}/bin/todo.sh list && sleep ${
          toString sleepSeconds
        }"
      '';
    }
    {
      block = "custom";
      interval = 30;
      command = pkgs.writers.writeDash "rss-new" ''
        ${pkgs.newsboat}/bin/newsboat --execute=print-unread | ${pkgs.gawk}/bin/awk '{ print "📰 " $1 }'
      '';
      on_click = pkgs.writers.writeDash "rss-update" ''
        ${pkgs.libnotify}/bin/notify-send --app-name=" Newsboat" "Updating feeds." \
          && ${pkgs.newsboat}/bin/newsboat --execute=reload \
          && ${pkgs.libnotify}/bin/notify-send --app-name=" Newsboat" "Feeds updated."
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
    {
      block = "net";
      device = wirelessInterface;
      speed_up = false;
      speed_down = false;
      signal_strength = true;
      ssid = true;
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
      alias = "💽";
    }
    {
      block = "memory";
      display_type = "memory";
      format_mem = "{Mug}G";
      clickable = false;
    }
    { block = "load"; }
    {
      block = "time";
      interval = 1;
      format = "%Y-%m-%d (%V %a) %H:%M";
    }
  ];
}
