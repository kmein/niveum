{ wifi-interface, colours, batteryBlock }:
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
  icons.name = "awesome";
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
