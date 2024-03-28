{
  pkgs,
  config,
  ...
}: {
  age.secrets = {
    miniflux-api-token = {
      file = ../secrets/miniflux-api-token.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  home-manager.users.me = {
    programs.i3status-rust = {
      enable = true;
      bars.bottom = {
        icons = "awesome6";
        settings = {
          theme.overrides = let
            colours = config.lib.stylix.colors.withHashtag;
          in {
            idle_bg = colours.base00;
            idle_fg = colours.base05;
            good_bg = colours.base00;
            good_fg = colours.base0B;
            warning_bg = colours.base00;
            warning_fg = colours.base0A;
            critical_bg = colours.base00;
            critical_fg = colours.base09;
            info_bg = colours.base00;
            info_fg = colours.base04;
            separator_bg = colours.base00;
            separator = " ";
          };
        };
        blocks = [
          {
            block = "music";
            format = "{$icon $combo $play |}";
            separator = " – ";
          }
          {
            block = "net";
            format = " $icon HU";
            missing_format = "";
            device = "ppp0";
          }
          {
            block = "net";
            format = " $icon FU";
            missing_format = "";
            device = "tun0";
          }
          {
            block = "battery";
            device = config.niveum.batteryName;
          }
          {
            block = "sound";
          }
          {
            block = "disk_space";
            format = "$icon $available";
          }
          {
            block = "memory";
            format = "$icon $mem_used.eng(prefix:G)";
          }
          {block = "load";}
          {
            block = "time";
            format = "$icon $timestamp.datetime(f:'%Y-%m-%d (%W %a) %H:%M', l:de_DE)";
          }
        ];
      };
    };
  };
}
