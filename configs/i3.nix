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
            criteria = { class = "floating"; };
            command = "floating enable";
          }
          {
            criteria = { class = "fzfmenu"; };
            command = "floating enable";
          }
          {
            criteria = { class = "mpv"; };
            command = lib.strings.concatStringsSep ", " [
              "floating enable"
              "sticky enable"
              "fullscreen disable"
              "resize set 640 480"
              "move position mouse"
            ];
          }
        ];
      };
      gaps.inner = 8;
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
        fonts = [ "Monospace ${toString (config.niveum.fonts.size - 1)}" ];
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
            inherit pkgs;
          })
        }";
      }];
      keybindings = {
        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";

        "${modifier}+Shift+b" = "move window to workspace prev";
        "${modifier}+Shift+n" = "move window to workspace next";
        "${modifier}+Shift+x" = "exec --no-startup-id ${move-to-new-workspace}";
        "${modifier}+b" = "workspace prev";
        "${modifier}+n" = "workspace next";
        "${modifier}+x" = "exec --no-startup-id ${new-workspace}";

        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+q" = "kill";
        "${modifier}+Shift+r" = "restart";

        "${modifier}+Shift+s" = "sticky toggle";
        "${modifier}+Shift+z" = "floating toggle";
        "${modifier}+c" = "split h";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+s" = "layout stacking";
        "${modifier}+v" = "split v";
        "${modifier}+w" = "layout tabbed";

        "${modifier}+Return" = "exec i3-sensible-terminal";
        "${modifier}+Shift+y" = "exec ${pkgs.qutebrowser}/bin/qutebrowser";
        "${modifier}+t" = "exec ${applications.fileManager}";
        "${modifier}+y" = "exec x-www-browser";

        "${modifier}+Shift+w" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
        "${modifier}+a" = "exec --no-startup-id ${pkgs.rofi}/bin/rofi -display-window — -show window";
        "${modifier}+d" = "exec --no-startup-id ${pkgs.dmenu}/bin/dmenu_run";
        "${modifier}+Shift+d" = "exec ${pkgs.writers.writeDash "notemenu" ''
          set -efu
          PATH=$PATH:${lib.makeBinPath [ pkgs.dmenu pkgs.findutils ]}

          cd ~/notes
          find . -maxdepth 1 -type f | dmenu -i -l 20 | xargs i3-sensible-terminal -e "$EDITOR"
        ''}";
        "${modifier}+p" = "exec --no-startup-id ${pkgs.pass}/bin/passmenu -l 5";
        "${modifier}+u" = "exec ${pkgs.scripts.emoji-menu}/bin/emoji-menu";

        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t";
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5";
        "XF86Calculator" = "exec ${pkgs.st}/bin/st -c floating -e ${pkgs.bc}/bin/bc";
        "XF86ScreenSaver" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
        "XF86Display" = "exec ${pkgs.xcalib}/bin/xcalib -invert -alter";

        # key names detected with xorg.xev:
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
