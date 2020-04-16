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
    /*
    overrides = {
      time = "📅";
      music = "🎵";
      music_play = "▶";
      music_pause = "";
      music_next = "⏭";
      music_prev = "⏮";
      cogs = "🎛";
      memory_mem = "📈";
      memory_swap = "📉";
      cpu = "🚦";
      bat = "🔋";
      bat_full = "⚡";
      bat_charging = "🔌";
      bat_discharging = "🔋";
    };
    */
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
          | ${pkgs.gawk}/bin/awk -F'│' '{print "CORONA " $3 " (" $4 ") †" $5 " (" $6 ")"}'
      '';
    }
    {
      block = "custom";
      interval = 2 * 60;
      command = pkgs.writers.writeDash "rss" ''
        ${pkgs.newsboat}/bin/newsboat -u "$NEWSBOAT_HOME/urls" -x print-unread | ${pkgs.gawk}/bin/awk '{ print "RSS " $1 }'
      '';
      on_click = pkgs.writers.writeDash "updateNewsboat" ''
        ${pkgs.newsboat}/bin/newsboat -u "$NEWSBOAT_HOME/urls" -x reload && ${pkgs.libnotify}/bin/notify-send newsboat "Feeds updated."
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
