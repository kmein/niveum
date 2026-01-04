{
  pkgs,
  lib,
  config,
  ...
}:
let
  stylixColors = config.lib.stylix.colors;
  ashellConfig = (pkgs.formats.toml { }).generate "config.toml" {
    # position = "bottom";
    modules = {
      left = [
        "Workspaces"
        [
          "WindowTitle"
        ]
      ];
      center = [ "Clock" ];
      right = [
        "KeyboardLayout"
        [
          "Tray"
          "SystemInfo"
          "Settings"
          "CustomNotifications"
        ]
      ];
    };
    workspaces = {
      workspace_names = [
        "١"
        "٢"
        "٣"
        "٤"
        "٥"
        "٦"
        "٧"
        "٨"
        "٩"
        "١٠"
      ];
      visibility_mode = "MonitorSpecific";
      enable_workspace_filling = false;
      disable_special_workspaces = true;
    };
    keyboard_layout.labels = {
      "de" = "🇩🇪";
    };
    window_title = {
      mode = "Title";
      truncate_title_after_length = 75;
    };
    media_player = {
      max_title_length = 40;
    };
    system_info.indicators = [
      "Cpu"
      "Memory"
      { Disk = "/"; }
    ];
    clock.format = "%Y-%m-%d (%W %a) %H:%M";
    settings.indicators = [
      "IdleInhibitor"
      "PowerProfile"
      "Audio"
      "Bluetooth"
      "Network"
      "Vpn"
      "Battery"
    ];
    appearance = {
      font_name = config.stylix.fonts.sansSerif.name;
      backdrop = 0.3;
      scale_factor = 0.75;
      # style = "Solid";
      primary_color = "#" + stylixColors.base0D;
      success_color = "#" + stylixColors.base0B;
      text_color = "#" + stylixColors.base05;
      workspace_colors = [ ("#" + stylixColors.base0E) ];
      background_color = "#" + stylixColors.base00;
      danger_color = "#" + stylixColors.base08;
      secondary_color = "#" + stylixColors.base0A;
    };

    CustomModule = [
      {
        name = "CustomNotifications";
        icon = "🔔";
        command = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -t -sw";
        listen_cmd = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -swb";
        icons."dnd.*" = "🔕";
        alert = ".*notification";
      }
    ];
  };
in
{
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-hyprland ];
  };

  home-manager.users.me = {
    home.file.".config/ashell/config.toml".source = ashellConfig;

    services.swaync.enable = true;

    services.mako.enable = true;

    services.clipman.enable = true;

    services.hyprsunset.enable = true;

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
          lock_cmd = "hyprlock";
        };
        listener = [
          {
            timeout = 900;
            on-timeout = "hyprlock";
          }
          {
            timeout = 1200;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = !config.programs.hyprland.enable;
      settings =
        let
          mod = "SUPER";
        in
        {
          env = [
            "XCURSOR_SIZE,${toString config.stylix.cursor.size}" # TODO
            "HYPRCURSOR_SIZE,${toString config.stylix.cursor.size}" # TODO
            "HYPRCURSOR_THEME,${config.stylix.cursor.name}"
            "QT_QPA_PLATFORM=wayland"
            "GDK_BACKEND=wayland"
            "HYPRSHOT_DIR=${config.home-manager.users.me.xdg.userDirs.download}/Screenshots"
          ];
          permission = [
            "${lib.getExe pkgs.hyprshot}, screencopy, allow"
            "${pkgs.xdg-desktop-portal-hyprland}/libexec/.xdg-desktop-portal-hyprland-wrapped, screencopy, allow"
          ];
          monitor = ",preferred,auto,auto"; # TODO https://wiki.hypr.land/Configuring/Monitors/
          exec-once = [
            (lib.getExe pkgs.ashell)
            "hyprctl dispatch exec \"[workspace special:obsidian silent] obsidian\""
          ];

          general = {
            gaps_in = 2;
            gaps_out = 2;
            border_size = 1;
            resize_on_border = true;
            allow_tearing = false;
            layout = "dwindle";
          };

          decoration = {
            rounding = 2;
            rounding_power = 2;
            active_opacity = 1.0;
            inactive_opacity = 0.9;
            shadow = {
              enabled = true;
              range = 4;
              render_power = 3;
            };
            blur = {
              enabled = true;
              size = 3;
              passes = 1;
              vibrancy = 0.17;
            };
          };

          animations = {
            enabled = false;
          };

          dwindle = {
            pseudotile = true;
            preserve_split = true;
          };

          master.new_status = "master";

          gesture = [
            "3, horizontal, workspace"
          ];

          input = {
            kb_layout = "de";
            kb_variant = "T3";
            kb_options = "compose:caps,grp:ctrls_toggle";
            follow_mouse = 1;
            sensitivity = 0;
            touchpad.natural_scroll = false;
          };

          bindm = [
            "${mod}, mouse:272, movewindow"
            "${mod}, mouse:273, resizewindow"
          ];
          bindel = [
            ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
            ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
            ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
            ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
            ",XF86MonBrightnessUp, exec, brightnessctl -e4 -n2 set 5%+"
            ",XF86MonBrightnessDown, exec, brightnessctl -e4 -n2 set 5%-"
            ", Print, exec, ${lib.getExe pkgs.hyprshot} -m region --clipboard-only"
            "${mod}, Print, exec, ${lib.getExe pkgs.hyprshot} -m region"
          ];
          bindl = [
            ", XF86AudioNext, exec, playerctl next"
            ", XF86AudioPause, exec, playerctl play-pause"
            ", XF86AudioPlay, exec, playerctl play-pause"
            ", XF86AudioPrev, exec, playerctl previous"
          ];
          bind = [
            "${mod}, Return, exec, ${lib.getExe pkgs.niveum-terminal}"
            "${mod} SHIFT, Q, killactive,"
            "${mod} SHIFT, R, exit,"
            "${mod}, t, exec, ${lib.getExe pkgs.niveum-filemanager}"
            "${mod}, Y, exec, ${lib.getExe pkgs.niveum-browser}"
            "${mod}, Q, exec, ${lib.getExe pkgs.clipman} pick --tool=${lib.getExe pkgs.rofi}"
            "${mod}, u, exec, ${lib.getExe pkgs.unicodmenu}"
            "${mod}, p, exec, rofi-pass"
            "${mod} Shift, Z, togglefloating,"
            "${mod}, D, exec, ${lib.getExe pkgs.rofi} -show run"
            "${mod}, P, pseudo," # dwindle
            "${mod}, v, togglesplit," # dwindle
            "${mod}, F, fullscreen"
            "${mod}, h, movefocus, l"
            "${mod}, l, movefocus, r"
            "${mod}, k, movefocus, u"
            "${mod}, j, movefocus, d"
            "${mod} SHIFT, H, movewindow, l"
            "${mod} SHIFT, L, movewindow, r"
            "${mod} SHIFT, K, movewindow, u"
            "${mod} SHIFT, J, movewindow, d"
            "${mod}, S, togglespecialworkspace, magic"
            "${mod} SHIFT, S, movetoworkspace, special:magic"
            "${mod}, O, togglespecialworkspace, obsidian"
            "${mod} SHIFT, O, movetoworkspace, special:obsidian"
          ]
          ++ lib.concatMap (
            i:
            let
              key = lib.mod i 10;
            in
            [
              "${mod}, ${toString key}, workspace, ${toString i}"
              "${mod} SHIFT, ${toString key}, movetoworkspace, ${toString i}"
            ]
          ) (lib.range 1 10);

          windowrule = [
            "suppressevent maximize, class:.*" # ignore maximize requests from apps
            "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0" # fix some dragging issues with wayyland

            "float,title:^(Picture-in-Picture)$"
            "pin,title:^(Picture-in-Picture)$"
            "size 640 360,title:^(Picture-in-Picture)$"
            "move 100%-640 100%-360,title:^(Picture-in-Picture)$"
          ];
        };
    };
  };
}
