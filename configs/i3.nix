{ config, pkgs, lib, ... }:
let
  new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  move-to-new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg move container to workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
in with config.niveum; {
  services.xserver = {
    windowManager.default = "i3";
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };

  home-manager.users.me.xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      fonts = [ "Monospace ${toString config.niveum.fonts.size}" ];
      modifier = "Mod4";
      window = {
        titlebar = false;
        border = 1;
        hideEdgeBorders = "smart";
        commands = [
          {
            command = "floating enable";
            criteria = { class = "fzfmenu"; };
          }
        ];
      };
      gaps = {
        inner = 8;
        outer = 0;
      };
      floating = {
        titlebar = false;
        border = 1;
      };
      colors =
        let scheme = { background = colours.background; text = colours.foreground; };
        in rec {
          focused = scheme // {
            border = colours.foreground;
            indicator = colours.foreground;
            childBorder = colours.foreground;
          };
          unfocused = scheme // {
            border = colours.background;
            indicator = colours.background;
            childBorder = colours.background;
          };
          focusedInactive = unfocused;
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
        fonts = [ "Monospace ${toString (config.niveum.fonts.size - 3)}" ];
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
        statusCommand = "${pkgs.unstable.i3status-rust}/bin/i3status-rs ${
          pkgs.writeTOML (import <niveum/dot/i3status-rust.nix> {
            wifi-interface = networkInterfaces.wireless;
            batteryBlock = batteryBlocks.default;
            inherit (config.niveum) colours;
          })
        }";
      }];
      keybindings = {
        "${modifier}+Down" = "focus down";
        "${modifier}+Left" = "focus left";
        "${modifier}+Return" = "exec i3-sensible-terminal";
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
        "${modifier}+d" = "exec ${pkgs.dmenu}/bin/dmenu_path | ${pkgs.scripts.fzfmenu}/bin/fzfmenu | \${SHELL:-/bin/sh} &";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+n" = "workspace next";
        "${modifier}+p" = "exec ${pkgs.rofi-pass}/bin/rofi-pass";
        "${modifier}+q" = "exec ${pkgs.qutebrowser}/bin/qutebrowser";
        "${modifier}+r" = "mode resize";
        "${modifier}+s" = "layout stacking";
        "${modifier}+t" = "exec ${applications.fileManager}";
        "${modifier}+v" = "split v";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+x" = "exec --no-startup-id ${new-workspace}";
        "${modifier}+y" = "exec x-www-browser";
        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t";
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5";
        "XF86Calculator" = "exec i3-sensible-terminal -e ${pkgs.python3}/bin/python3";
        "XF86ScreenSaver" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
        "XF86Display" = "exec ${pkgs.xcalib}/bin/xcalib -invert -alter";
        # XF86WakeUp (fn twice)
        # XF86Battery (fn f3)
        # XF86Sleep (fn f4) - actually suspends
        # XF86WLAN
        # XF86WebCam (fn f6)
        # XF86TouchpadToggle (fn f8)
        # XF86Suspend (fn f12) - actually suspends to disk
        # Num_Lock (fn Roll) - numlocks
        # XF86Audio{Prev,Next,Mute,Play,Stop}
        # XF86Forward
        # XF86Back
        # XF86Launch1 (thinkvantage)
      };
    };
  };
}
