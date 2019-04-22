{ config, pkgs, lib, ... }:
let
  new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  move-to-new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg move container to workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  wifi-interface = { scardanelli = "wlp2s0"; homeros = "wlp3s0"; }.${config.networking.hostName};
in with config.niveum; {
  services.xserver = {
    windowManager.default = "i3";
    windowManager.i3.enable = true;
  };

  home-manager.users.me.xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      fonts = [ "${config.niveum.fonts.ui.name} ${toString config.niveum.fonts.ui.size}" ];
      modifier = "Mod4";
      window = {
        titlebar = false;
        border = 1;
        hideEdgeBorders = "smart";
      };
      floating = {
        titlebar = false;
        border = 1;
      };
      colors =
        let scheme = { background = colours.background; text = colours.foreground; };
        in rec {
          focused = scheme // {
            border = colours.background;
            indicator = colours.background;
            childBorder = colours.background;
          };
          focusedInactive = focused;
          unfocused = focused;
          urgent = scheme // {
            border = colours.red.bright;
            indicator = colours.red.bright;
            childBorder = colours.red.bright;
          };
          placeholder = scheme // {
            border = colours.green.bright;
            indicator = colours.green.bright;
            childBorder = colours.green.bright;
          };
        };
      bars = [{
        workspaceButtons = false;
        fonts = [ "${config.niveum.fonts.terminal.name} ${toString config.niveum.fonts.terminal.size}" ];
        mode = "hide";
        colors = rec {
          background = colours.background;
          separator = background;
          statusline = colours.foreground;
          bindingMode = {
            background = colours.red.bright;
            border = colours.background;
            text = colours.foreground;
          };
        };
        statusCommand = "${pkgs.i3status}/bin/i3status -c ${pkgs.writeText "i3status.conf" ''
          general {
            colors = true
            color_good = "${colours.green.dark}"
            color_bad = "${colours.red.dark}"
            color_degraded = "${colours.white.dark}"
            interval = 5
            separator = " "
          }

          order += "run_watch retiolum"
          order += "path_exists openvpn"
          order += "wireless ${wifi-interface}"
          order += "battery all"
          order += "volume master"
          order += "load"
          order += "tztime local"

          wireless ${wifi-interface} {
            format_up = "%essid"
            format_down = "offline"
          }

          run_watch retiolum {
            pidfile = "/var/run/tinc.retiolum.pid"
            format = "%title"
          }

          path_exists openvpn {
            path = "/proc/sys/net/ipv4/conf/tun0"
            format = "%title"
          }

          battery all {
            format = "%status%percentage"
            format_down = "No battery"
            status_chr = "↑"
            status_bat = "↓"
            status_unk = ""
            status_full = "↯"
            path = "/sys/class/power_supply/BAT%d/uevent"
            low_threshold = 15
            threshold_type = "percentage"
            integer_battery_capacity = true
          }

          volume master {
            format = "%volume"
            format_muted = "%volume"
            device = "default"
            mixer = "Master"
            mixer_idx = 0
          }

          tztime local {
            format = "%Y-%m-%d %H:%M"
          }

          load {
            format = "%1min"
          }''}";
      }];
      keybindings = {
        "${modifier}+Down" = "focus down";
        "${modifier}+Left" = "focus left";
        "${modifier}+Return" = "exec ${applications.terminal}";
        "${modifier}+Right" = "focus right";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+b" = "move window to workspace prev";
        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+n" = "move window to workspace next";
        "${modifier}+Shift+q" = "kill";
        "${modifier}+Shift+r" = "restart";
        "${modifier}+Shift+w" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
        "${modifier}+Shift+x" = "exec --no-startup-id ${move-to-new-workspace}";
        "${modifier}+Shift+z" = "floating toggle";
        "${modifier}+Up" = "focus up";
        "${modifier}+a" = "exec ${pkgs.rofi}/bin/rofi -display-window — -show window";
        "${modifier}+b" = "workspace prev";
        "${modifier}+c" = "split h";
        "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -display-run — -show run";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+n" = "workspace next";
        "${modifier}+p" = "exec ${pkgs.rofi-pass}/bin/rofi-pass";
        "${modifier}+r" = "mode resize";
        "${modifier}+s" = "layout stacking";
        "${modifier}+t" = "exec ${applications.fileManager}";
        "${modifier}+v" = "split v";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+x" = "exec --no-startup-id ${new-workspace}";
        "${modifier}+y" = "exec ${applications.browser}";
        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t";
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5";
      };
    };
  };
}
