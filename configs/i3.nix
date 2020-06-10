{ config, pkgs, lib, ... }:
let
  klem = import <niveum/packages/scripts/klem.nix> {
    inherit pkgs lib;
    config.scripts = {
      "p.r" = pkgs.writers.writeDash "p.r" ''
        ${pkgs.curl}/bin/curl -fSs http://p.r --data-binary @- \
          | ${pkgs.coreutils}/bin/tail --lines=1 \
          | ${pkgs.gnused}/bin/sed 's/\\<r\\>/krebsco.de/'
      '';
      "ix.io" = pkgs.writers.writeDash "ix.io" ''
        ${pkgs.curl}/bin/curl -fSs -F 'f:1=<-' ix.io
      '';
      "go.r" = pkgs.writers.writeDash "go.r" ''
        ${pkgs.curl}/bin/curl -fSs http://go.r -F "uri=$(${pkgs.coreutils}/bin/cat)"
      '';
      "0x0.st" = pkgs.writers.writeDash "0x0.st" ''
        ${pkgs.curl}/bin/curl -fSs https://0x0.st -F "shorten=$(${pkgs.coreutils}/bin/cat)"
      '';
      "rot13" = pkgs.writers.writeDash "rot13" ''
        ${pkgs.coreutils}/bin/tr '[A-Za-z]' '[N-ZA-Mn-za-m]'
      '';
      "ipa" = pkgs.writers.writeDash "ipa" ''
        ${scripts.ipa}/bin/ipa
      '';
      "rot13.hs" = pkgs.writers.writeHaskell "rot13.hs" {} ''
        import Data.Char (chr, isAlpha, ord, toLower)
        import Data.Bool (bool)

        main = interact $ map $ \c ->
          if isAlpha c
          then chr $ bool (-) (+) ('m' >= toLower c) (ord c) 13
          else c
      '';
      "rot13.py" = pkgs.writers.writePython3 "rot13.py" {} ''
        import codecs
        import sys
        sys.stdout.write(codecs.encode(sys.stdin.read(), "rot13"))
      '';
    };
  };

  scripts = import <niveum/packages/scripts> { inherit pkgs lib; };
  myLib = import <niveum/lib> { inherit pkgs; };
  inherit (myLib) writeTOML;

  new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  move-to-new-workspace = pkgs.unstable.writers.writeDash "new-workspace" ''
    i3-msg move container to workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';

  # https://github.com/LukeSmithxyz/voidrice/blob/9fe6802122f6e0392c7fe20eefd30437771d7f8e/.local/bin/dmenuunicode
  emoji-menu =
  let emoji-file = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/LukeSmithxyz/voidrice/master/.local/share/larbs/emoji";
    sha256 = "09m2rgb9d5jpiy8q4jz3dw36gkpb4ng2pl7xi7ppsrzzzdvq85qk";
  };
  in with pkgs; writers.writeDashBin "emoji-menu" ''
    PATH=${lib.makeBinPath [ coreutils dmenu gnused libnotify xclip xdotool ]}
    chosen=$(cut -d ';' -f1 ${emoji-file} | dmenu -i -l 10 | sed "s/ .*//")

    [ "$chosen" != "" ] || exit

    echo "$chosen" | tr -d '\n' | xclip -selection clipboard

    if [ -n "$1" ]; then
      xdotool key Shift+Insert
    else
      notify-send --app-name="emoji-menu" "'$chosen' copied to clipboard." &
    fi
  '';

in with config.niveum; {
  services.xserver = {
    displayManager.defaultSession = "none+i3";
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
        mode = "dock";
        position = "top";
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
          writeTOML (import <niveum/dot/i3status-rust.nix> {
            wifi-interface = networkInterfaces.wireless;
            batteryBlock = batteryBlocks.default;
            inherit (config.niveum) colours;
            inherit pkgs;
          })
        }";
      }];
      modes.resize = {
        "Escape" = "mode \"default\"";
        "Return" = "mode \"default\"";
        "h" = "resize shrink width 10 px or 5 ppt";
        "j" = "resize grow height 10 px or 5 ppt";
        "k" = "resize shrink height 10 px or 5 ppt";
        "l" = "resize grow width 10 px or 5 ppt";
      };
      keybindings = {
        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";

        "${modifier}+Menu" = "exec ${klem}/bin/klem";

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
        "${modifier}+Shift+Return" = "exec alacritty-flipped";
        # "${modifier}+Shift+y" = "exec ${pkgs.qutebrowser}/bin/qutebrowser";
        "${modifier}+t" = "exec ${applications.fileManager}";
        "${modifier}+y" = "exec x-www-browser";

        "${modifier}+Shift+w" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";
        "${modifier}+a" = "exec --no-startup-id ${pkgs.rofi}/bin/rofi -display-window — -show window";
        "${modifier}+d" = "exec --no-startup-id ${pkgs.dmenu}/bin/dmenu_run";
        "${modifier}+Shift+d" = "exec ${pkgs.writers.writeDash "notemenu" ''
          set -efu
          PATH=$PATH:${lib.makeBinPath [ pkgs.dmenu pkgs.findutils pkgs.coreutils ]}

          cd ~/notes
          find . -type f -printf "%T@ %p\n" \
            | sort --reverse --numeric-sort \
            | cut --delimiter=" " --fields=2 \
            | dmenu -i \
            | xargs i3-sensible-terminal -e "$EDITOR"
        ''}";
        "${modifier}+p" = "exec --no-startup-id ${pkgs.pass}/bin/passmenu -l 5";
        "${modifier}+u" = "exec ${emoji-menu}/bin/emoji-menu";

        "${modifier}+F9" = "exec ${pkgs.redshift}/bin/redshift -O 4000 -b 0.85";
        "${modifier}+F10" = "exec ${pkgs.redshift}/bin/redshift -x";
        "${modifier}+F11" = "exec ${pkgs.xcalib}/bin/xcalib -invert -alter";

        "Print" = "exec flameshot-once";
        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t";
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5";
        "XF86Calculator" = "exec ${pkgs.st}/bin/st -c floating -e ${pkgs.bc}/bin/bc";
        "XF86AudioPause" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl pause";
        "XF86AudioPlay" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl previous";
        "XF86ScreenSaver" = "exec ${pkgs.xautolock}/bin/xautolock -locknow";

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
