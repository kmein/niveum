{ wifi-interface, colours, batteryBlock }:
{
  theme = {
    name = "plain";
    overrides = {
      separator = " ";
      critical_fg = colours.red.bright;
      good_fg = colours.green.bright;
      idle_fg = colours.foreground;
      info_fg = colours.foreground;
      separator_fg = "auto";
      warning_fg = colours.yellow.bright;
      alternating_tint_bg = colours.background;
      alternating_tint_fg = colours.background;
      critical_bg = colours.background;
      good_bg = colours.background;
      idle_bg = colours.background;
      info_bg = colours.background;
      separator_bg = "auto";
      warning_bg = colours.background;
    };
  };
  icons = {
    name = "none";
    overrides = {
      music = "♫ ";
      music_play = "►";
      music_next = "→";
      music_prev = "←";
      volume_full = "♪ ";
      volume_half = "♪ ";
      volume_empty = "♪ ";
      bat =  "⚡ ";
      bat_full = "⚡ ";
      bat_charging = "↑ ";
      bat_discharging = "↓ ";
      memory_mem = "";
      separator = "";
      net_wireless = "";
      cogs = "";
      time = "";
    };
  };
  block = [
    {
      block = "music";
      player = "spotify";
      buttons = ["prev" "play" "next"];
      marquee = false;
      max_width = 35;
    }
    {
      block = "net";
      device = wifi-interface;
      ssid = true;
      interval = 1;
      speed_up = false;
      speed_down = false;
    }
    {
      block = "battery";
      device = batteryBlock;
      show = "both";
    }
    {
      block = "sound";
    }
    {
      block = "memory";
      display_type = "memory";
      format_mem = "{MUg}G";
      clickable = false;
    }
    {
      block = "load";
      interval = 1;
      format = "{1m}";
    }
    {
      block = "time";
      interval = 5;
      format = "%Y-%m-%d %H:%M";
    }
  ];
}
