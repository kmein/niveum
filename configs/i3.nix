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
    windowManager.i3.enable = true;
  };

  home-manager.users.me.xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      fonts = [ "Sans ${toString config.niveum.fonts.size}" ];
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
        fonts = [ "Monospace ${toString config.niveum.fonts.size}" ];
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
        statusCommand = "${pkgs.unstable.i3status-rust}/bin/i3status-rs ${pkgs.writeText "i3status-rust.toml" (
          import <dot/i3status-rust.nix> {
            wifi-interface = networkInterfaces.wireless;
            inherit (config.niveum) colours;
          }
        )}";
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
        "XF86Calculator" = "exec ${applications.terminal} -e ${pkgs.python3}/bin/python3";
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
