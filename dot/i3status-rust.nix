{ pkgs, wifi-interface, colours, batteryBlock }:
{
  theme = {
    name = "plain";
    overrides = {
      critical_fg = colours.red.bright;
      good_fg = colours.green.bright;
      idle_fg = colours.foreground;
      info_fg = colours.foreground;
      warning_fg = colours.yellow.bright;
      alternating_tint_bg = colours.background;
      alternating_tint_fg = colours.background;
      critical_bg = colours.background;
      good_bg = colours.background;
      idle_bg = colours.background;
      info_bg = colours.background;
      warning_bg = colours.background;
    };
  };
  icons = {
    name = "none";
    overrides = {
      time = "📅 ";
      music = "🎵";
      music_play = "▶";
      music_pause = "";
      music_next = "⏭";
      music_prev = "⏮";
      cpu = "🖥 ";
      memory_mem = "🧠 ";
      cogs = "🚦 ";
      bat = "🔋";
      bat_full = "⚡";
      bat_charging = "🔌";
      bat_discharging = "🔋";
      bat_quarter = "🔋";
      bat_three_quarters = "🔋";
      net_up = "🌐";
      net_down = "❎";
      net_wireless = "📶";
      net_wired = "🌐";
      net_vpn = "🛡 ";
      toggle_off = "👎";
      toggle_on = "👍";
      volume_full = "🔊 ";
      volume_half = "🔉 ";
      volume_muted = "⛔";
      volume_empty = "🔈 ";
    };
  };
  block = [
    {
      block = "music";
      player = "spotify";
      buttons = ["prev" "play" "next"];
      marquee = false;
      max_width = 35;
      on_collapsed_click = "spotify";
    }
    {
      block = "custom";
      interval = 60 * 60 * 60; # hourly
      command = pkgs.writers.writeDash "corona" ''
        ${pkgs.curl}/bin/curl https://corona-stats.online/germany \
          | ${pkgs.gnugrep}/bin/grep Germany \
          | ${pkgs.gnused}/bin/sed 's/\s*//g' \
          | ${pkgs.ansifilter}/bin/ansifilter \
          | ${pkgs.gawk}/bin/awk -F'│' '{print "🤒 " $3 " 💀 " $5}'
      '';
    }
    {
      block = "custom";
      interval = 2 * 60;
      command = pkgs.writers.writeDash "rss" ''
        ${pkgs.newsboat}/bin/newsboat -x print-unread | ${pkgs.gawk}/bin/awk '{ print "📰 " $1 }'
      '';
      on_click = pkgs.writers.writeDash "updateNewsboat" ''
        ${pkgs.newsboat}/bin/newsboat -x reload && ${pkgs.libnotify}/bin/notify-send newsboat "Feeds updated."
      '';
    }
    {
      block = "custom";
      interval = 60;
      command = pkgs.writers.writeDash "tasks" ''
        ${pkgs.todo-txt-cli}/bin/todo.sh list | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print "⏳ " $2 }'
      '';
    }
    {
      block = "net";
      device = wifi-interface;
      speed_up = false;
      speed_down = false;
      signal_strength = true;
      ssid = true;
    }
    {
      block = "battery";
      device = batteryBlock;
      show = "both";
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
      format_mem = "{MUg}G";
      clickable = false;
    }
    {
      block = "load";
    }
    {
      block = "time";
      interval = 5;
      format = "%Y-%m-%d %H:%M";
    }
  ];
}
