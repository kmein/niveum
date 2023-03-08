{
  config,
  pkgs,
  lib,
  niveumPackages,
  ...
}: let
  inherit (import ../lib) defaultApplications colours;
  klem = niveumPackages.klem.override {
    config.dmenu = "${pkgs.dmenu}/bin/dmenu -i -p klem";
    config.scripts = {
      "p.r" = pkgs.writers.writeDash "p.r" ''
        ${pkgs.curl}/bin/curl -fSs http://p.r --data-binary @- \
          | ${pkgs.coreutils}/bin/tail --lines=1 \
          | ${pkgs.gnused}/bin/sed 's/\\<r\\>/krebsco.de/'
      '';
      # "envs.sh host" = pkgs.writers.writeDash "envs-host" ''
      #   ${pkgs.curl}/bin/curl -F "file=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
      # '';
      "envs.sh mirror" = pkgs.writers.writeDash "envs-mirror" ''
        ${pkgs.curl}/bin/curl -F "url=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
      '';
      "envs.sh shorten" = pkgs.writers.writeDash "envs-shorten" ''
        ${pkgs.curl}/bin/curl -F "shorten=$(${pkgs.coreutils}/bin/cat)" https://envs.sh
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
        ${niveumPackages.ipa}/bin/ipa
      '';
      "betacode" = pkgs.writers.writeDash "betacode" ''
        ${niveumPackages.betacode}/bin/betacode
      '';
      "curl" = pkgs.writers.writeDash "curl" ''
        ${pkgs.curl}/bin/curl -fSs "$(${pkgs.coreutils}/bin/cat)"
      '';
      ocr = pkgs.writers.writeDash "ocr" ''
        ${pkgs.tesseract4}/bin/tesseract -l eng+deu - stdout
      '';
    };
  };

  new-workspace = pkgs.writers.writeDash "new-workspace" ''
    i3-msg workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
  move-to-new-workspace = pkgs.writers.writeDash "new-workspace" ''
    i3-msg move container to workspace $(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
  '';
in {
  age.secrets = {
    github-token-i3status-rust = {
      file = ../secrets/github-token-i3status-rust.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    openweathermap-api-key = {
      file = ../secrets/openweathermap-api-key.age;
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  services.xserver = {
    displayManager.defaultSession = "none+i3";
    windowManager.i3 = {
      enable = true;
    };
  };

  services.xserver = {
    monitorSection = ''Option "DPMS" "false"'';
    serverFlagsSection = ''
      Option "BlankTime" "0"
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime" "0"
    '';
    extraConfig = ''
      Section "Extensions"
        Option "DPMS" "Disable"
      EndSection
    '';
  };

  home-manager.users.me.xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      fonts = {
        names = ["Sans"];
        size = 10.0;
      };
      modifier = "Mod4";
      window = {
        titlebar = false;
        border = 1;
        hideEdgeBorders = "smart";
        commands = [
          {
            criteria = {class = "floating";};
            command = "floating enable";
          }
          {
            criteria = {class = "fzfmenu";};
            command = "floating enable";
          }
          {
            criteria = {class = ".*";};
            command = "border pixel 2";
          }
          {
            criteria = {class = "mpv";};
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
      floating = {
        titlebar = false;
        border = 1;
      };
      colors = let
        scheme = {
          background = colours.background;
          text = colours.foreground;
        };
      in rec {
        focused =
          scheme
          // {
            border = colours.blue.bright;
            indicator = colours.blue.bright;
            childBorder = colours.blue.bright;
          };
        unfocused =
          scheme
          // {
            border = colours.background;
            indicator = colours.background;
            childBorder = colours.background;
          };
        focusedInactive = unfocused;
        urgent =
          scheme
          // {
            border = colours.red.bright;
            indicator = colours.red.bright;
            childBorder = colours.red.bright;
          };
        placeholder =
          scheme
          // {
            border = colours.green.bright;
            indicator = colours.green.bright;
            childBorder = colours.green.bright;
          };
      };
      bars = [
        {
          workspaceButtons = false;
          fonts = {
            names = ["Monospace" "Font Awesome 6 Free"];
            size = 8.0;
          };
          mode = "dock"; # "hide";
          position = "bottom";
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
          statusCommand = toString (pkgs.writers.writeDash "i3status-rust" ''
            export I3RS_GITHUB_TOKEN="$(cat ${config.age.secrets.github-token-i3status-rust.path})"
            export OPENWEATHERMAP_API_KEY="$(cat ${config.age.secrets.openweathermap-api-key.path})"
            ${pkgs.i3status-rust}/bin/i3status-rs ${
              (pkgs.formats.toml {}).generate "i3status-rust.toml" (import ../lib/i3status-rust.nix {
                inherit (config.niveum) batteryName wirelessInterface;
                inherit (config.home-manager.users.me.accounts.email) accounts;
                inherit colours;
                inherit pkgs;
              })
            }'');
        }
      ];
      modes.resize = {
        "Escape" = ''mode "default"'';
        "Return" = ''mode "default"'';
        "h" = "resize shrink width 10 px or 5 ppt";
        "j" = "resize grow height 10 px or 5 ppt";
        "k" = "resize shrink height 10 px or 5 ppt";
        "l" = "resize grow width 10 px or 5 ppt";
      };
      keybindings = {
        "${modifier}+Shift+h" = "move left 25 px";
        "${modifier}+Shift+j" = "move down 25 px";
        "${modifier}+Shift+k" = "move up 25 px";
        "${modifier}+Shift+l" = "move right 25 px";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";

        "${modifier}+Shift+b" = "move window to workspace prev";
        "${modifier}+Shift+n" = "move window to workspace next";
        "${modifier}+Shift+x" = "exec ${move-to-new-workspace}";
        "${modifier}+b" = "workspace prev";
        "${modifier}+n" = "workspace next";
        "${modifier}+x" = "exec ${new-workspace}";

        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+q" = "kill";
        "${modifier}+Shift+r" = "restart";

        "${modifier}+z" = "sticky toggle";
        "${modifier}+Shift+z" = "floating toggle";

        "${modifier}+s" = "scratchpad show";
        "${modifier}+Shift+s" = "move scratchpad";

        "${modifier}+c" = "split h";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+v" = "split v";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+q" = "exec ${pkgs.writers.writeDash "newsboat-sync" ''
          notify-send --app-name="newsboat" "Updating ..."
          newsboat -x reload
          notify-send --app-name="newsboat" "Finished updating."
        ''}";

        # "${modifier}+Shift+y" = "exec ${pkgs.qutebrowser}/bin/qutebrowser";
        "${modifier}+Return" = "exec ${(defaultApplications pkgs).terminal}";
        "${modifier}+t" = "exec ${(defaultApplications pkgs).fileManager}";
        "${modifier}+y" = "exec ${(defaultApplications pkgs).browser}";
        "${modifier}+0" = "exec ${niveumPackages.menu-calc}/bin/=";

        "${modifier}+Shift+w" = "exec ${niveumPackages.k-lock}/bin/k-lock";
        "${modifier}+d" = "exec ${pkgs.writers.writeDash "run" ''exec rofi -modi run,ssh,window -show run''}";
        "${modifier}+Shift+d" = "exec ${
          pkgs.writers.writeDash "notemenu" ''
            set -efu
            PATH=$PATH:${
              lib.makeBinPath [pkgs.rofi pkgs.findutils pkgs.coreutils]
            }

            cd ~/notes
            note_file=$({
              echo diary/$(date -I).md
              echo diary/$(date -I -d yesterday).md
              find . ! -name '.*' -type f -printf "%T@ %p\n" | sort --reverse --numeric-sort | cut --delimiter=" " --fields=2-
            } | rofi -dmenu -i -p 'notes')
            if test "$note_file"
            then
              alacritty --working-directory ~/notes -e "$EDITOR" "$note_file"
            fi
          ''
        }";
        "${modifier}+p" = "exec rofi-pass";
        "${modifier}+Shift+p" = "exec rofi-pass --insert";
        "${modifier}+u" = "exec ${niveumPackages.unicodmenu}/bin/unicodmenu";

        "${modifier}+F6" = "exec ${pkgs.xorg.xkill}/bin/xkill";
        "${modifier}+F7" = "exec ${pkgs.writers.writeDash "showkeys-toggle" ''
          if ${pkgs.procps}/bin/pgrep screenkey; then
            exec ${pkgs.procps}/bin/pkill screenkey
          else
            exec ${pkgs.screenkey}/bin/screenkey
          fi
        ''}";
        "${modifier}+F8" = "exec switch-theme toggle";
        "${modifier}+F9" = "exec ${pkgs.redshift}/bin/redshift -O 4000 -b 0.85";
        "${modifier}+F10" = "exec ${pkgs.redshift}/bin/redshift -x";
        "${modifier}+F11" = "exec ${pkgs.xcalib}/bin/xcalib -invert -alter";
        "${modifier}+F12" = "exec ${klem}/bin/klem";
        "Print" = "exec flameshot gui";
        "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 5";
        "XF86Calculator" = "exec ${pkgs.st}/bin/st -c floating -e ${pkgs.bc}/bin/bc";
        "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl pause";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "XF86AudioStop" = "exec ${pkgs.playerctl}/bin/playerctl stop";
        "XF86ScreenSaver" = "exec ${niveumPackages.k-lock}/bin/k-lock";

        "XF86Display" = "exec ${niveumPackages.dmenu-randr}/bin/dmenu-randr";

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
