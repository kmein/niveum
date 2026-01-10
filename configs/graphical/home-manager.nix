{
  lib,
  pkgs,
  config,
  ...
}:
let
  klem = pkgs.klem.override {
    options.dmenu = "${pkgs.dmenu}/bin/dmenu -i -p klem";
    options.scripts = {
      "p.r paste" = pkgs.writers.writeDash "p.r" ''
        ${pkgs.curl}/bin/curl -fSs http://p.r --data-binary @- \
          | ${pkgs.coreutils}/bin/tail --lines=1 \
          | ${pkgs.gnused}/bin/sed 's/\\<r\\>/krebsco.de/'
      '';
      "envs.sh paste" = pkgs.writers.writeDash "envs-host" ''
        ${pkgs.curl}/bin/curl -F "file=@-" https://envs.sh
      '';
      # this segfaults
      # "envs.sh mirror" = pkgs.writers.writeDash "envs-mirror" ''
      #   ${pkgs.curl}/bin/curl -F "url=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
      # '';
      "envs.sh shorten" = pkgs.writers.writeDash "envs-shorten" ''
        ${pkgs.curl}/bin/curl -F "shorten=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
      '';
      "go.r shorten" = pkgs.writers.writeDash "go.r" ''
        ${pkgs.curl}/bin/curl -fSs http://go.r -F "uri=$(${pkgs.coreutils}/bin/cat)"
      '';
      "4d2.org paste" = pkgs.writers.writeDash "4d2-paste" ''
        ${pkgs.curl}/bin/curl -F "file=@-" https://depot.4d2.org/
      '';
      "0x0.st shorten" = pkgs.writers.writeDash "0x0.st" ''
        ${pkgs.curl}/bin/curl -fSs https://0x0.st -F "shorten=$(${pkgs.coreutils}/bin/cat)"
      '';
      "rot13" = pkgs.writers.writeDash "rot13" ''
        ${pkgs.coreutils}/bin/tr '[A-Za-z]' '[N-ZA-Mn-za-m]'
      '';
      "ipa" = pkgs.writers.writeDash "ipa" ''
        ${pkgs.ipa}/bin/ipa
      '';
      "betacode" = pkgs.writers.writeDash "betacode" ''
        ${pkgs.betacode}/bin/betacode
      '';
      "curl" = pkgs.writers.writeDash "curl" ''
        ${pkgs.curl}/bin/curl -fSs "$(${pkgs.coreutils}/bin/cat)"
      '';
      ocr = pkgs.writers.writeDash "ocr" ''
        ${pkgs.tesseract4}/bin/tesseract -l eng+deu - stdout
      '';
      emojai = pkgs.writers.writeDash "emojai" ''
        ${pkgs.curl}/bin/curl https://www.emojai.app/api/generate -X POST -H 'Content-Type: application/json' --data-raw "$(${pkgs.jq}/bin/jq -sR '{emoji:.}')" | ${pkgs.jq}/bin/jq -r .result
      '';
    };
  };

  arabic.workspaces = [
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
  arabic.music = "الموسيقى";
  arabic.obsidian = "السبج";
  hindi.workspaces = [
    "१"
    "२"
    "३"
    "४"
    "५"
    "६"
    "७"
    "८"
    "९"
    "१०"
  ];
  hindi.music = "संगीत";
  hindi.obsidian = "उपलविशेषः";
  hebrew.workspaces = [
    "א"
    "ב"
    "ג"
    "ד"
    "ה"
    "ו"
    "ז"
    "ח"
    "ט"
    "י"
  ];
  hebrew.music = "מוסיקה";
  hebrew.obsidian = "אובסידיאן";

  latin.workspaces = [
    "Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ"
  ];
  latin.music = "MVSICA";
  latin.obsidian = "NOSCENDA";

  greek.workspaces = [
    "Α" "Β" "Γ" "Δ" "Ε" "Ϛ" "Ζ" "Η" "Θ" "Ι"
  ];
  greek.music = "ΜΟΥΣΙΚΗ";
  greek.obsidian = "ΥΠΟΜΝΗΜΑΤΑ";

  language = greek;
in
{
  services.mako = {
    enable = true;
    settings.default-timeout = 10 * 1000;
  };

  services.hyprsunset.enable = true;

  programs.ashell = {
    enable = true;
    settings = {
      # position = "bottom";
      modules = {
        left = [
          "Workspaces"
          [
            "WindowTitle"
          ]
          [
            "MediaPlayer"
          ]
        ];
        center = [ "Clock" ];
        right = [
          "KeyboardLayout"
          [
            "Tray"
            "SystemInfo"
            "Settings"
          ]
        ];
      };
      workspaces = {
        enable_workspace_filling = false;
        disable_special_workspaces = true;
        visibility_mode = "MonitorSpecific";
        workspace_names = language.workspaces;
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
        scale_factor = 0.85;
        # style = "Solid";
      };
    };
  };

  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = "on";
      splash = false;
      preload = [ "${config.users.users.me.home}/.cache/wallpaper/wallpaper" ];
    };
  };

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

  programs.hyprlock = {
    enable = true;
    settings = {
      animations.enabled = false;
      general = {
        hide_cursor = true;
        ignore_empty_input = true;
      };
    };
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };

  wayland.windowManager.hyprland =
    let
      mod = "SUPER";
    in
    {
      enable = true;
      systemd.enable = false;
      systemd.variables = [ "--all" ];
      settings = {
        env = [
          "XCURSOR_SIZE,${toString config.stylix.cursor.size}" # TODO
          "HYPRCURSOR_SIZE,${toString config.stylix.cursor.size}" # TODO
          "HYPRCURSOR_THEME,${config.stylix.cursor.name}"
          "QT_QPA_PLATFORM=wayland"
          "GDK_BACKEND=wayland"
          "NIXOS_OZONE_WL=1"
          "HYPRSHOT_DIR=${config.home-manager.users.me.xdg.userDirs.download}/screenshots"
        ];
        permission = [
          "${lib.getExe pkgs.hyprshot}, screencopy, allow"
          "${pkgs.xdg-desktop-portal-hyprland}/libexec/.xdg-desktop-portal-hyprland-wrapped, screencopy, allow"
        ];
        monitor = [
          ",preferred, 0x0, 1" # TODO https://wiki.hypr.land/Configuring/Monitors/
          "desc:Samsung Electric Company C27F390 HTQH602129, 1920x1080, 0x-1080, 1"
        ];
        exec-once = [
          (lib.getExe pkgs.ashell)
          "hyprctl dispatch exec \"[workspace special:${language.obsidian} silent] obsidian\""
          "${lib.getExe' pkgs.wl-clipboard "wl-paste"} -t text --watch ${lib.getExe pkgs.clipman} store"
          # (lib.getExe pkgs.hyprsunset)
          # (lib.getExe pkgs.hyprpaper)
        ];

        device = [
          {
            name = "elan-touchscreen";
            enabled = false;
          }
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
          inactive_opacity = 1.0;
          shadow = {
            enabled = false;
            range = 4;
            render_power = 3;
          };
          blur = {
            enabled = false;
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
          "${mod}, Q, exec, ${lib.getExe pkgs.clipman} pick --tool=rofi"
          "${mod}, u, exec, ${lib.getExe pkgs.unicodmenu}"
          "${mod}, p, exec, ${lib.getExe pkgs.rofi-pass-wayland}"
          "${mod} Shift, Z, togglefloating,"
          "${mod}, D, exec, ${lib.getExe pkgs.rofi} -show run"
          "${mod}, v, togglesplit," # dwindle
          "${mod} SHIFT, V, pseudo," # dwindle
          "${mod}, F, fullscreen"
          "${mod}, h, movefocus, l"
          "${mod}, l, movefocus, r"
          "${mod}, k, movefocus, u"
          "${mod}, j, movefocus, d"
          "${mod}, F9, exec, hyprctl hyprsunset temperature -1000"
          "${mod}, F10, exec, hyprctl hyprsunset temperature +1000" # reset color temperature

          "${mod}, F12, exec, ${klem}/bin/klem"
          "${mod} SHIFT, W, exec, hyprlock"
          "${mod} SHIFT, H, movewindow, l"
          "${mod} SHIFT, L, movewindow, r"
          "${mod} SHIFT, K, movewindow, u"
          "${mod} SHIFT, J, movewindow, d"
          "${mod}, S, togglespecialworkspace, ${language.music}"
          "${mod} SHIFT, S, movetoworkspace, special:${language.music}"
          "${mod}, O, togglespecialworkspace, ${language.obsidian}"
          "${mod} SHIFT, O, movetoworkspace, special:${language.obsidian}"
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
      extraConfig = ''
        bind = ${mod}, R, submap, resize
        submap = resize
        binde = , l, resizeactive, 50 0
        binde = , h, resizeactive, -50 0
        binde = , k, resizeactive, 0 -50
        binde = , j, resizeactive, 0 50
        bind = , escape, submap, reset
        submap = reset
      '';
    };
}
