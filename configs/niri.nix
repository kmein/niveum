{
  pkgs,
  config,
  niveumPackages,
  lib,
  ...
}:
let
  inherit (import ../lib) defaultApplications;
  niriConfig =
    let
      klem = niveumPackages.klem.override {
        config.dmenu = "${pkgs.dmenu}/bin/dmenu -i -p klem";
        config.scripts = {
          "p.r paste" = pkgs.writers.writeDash "p.r" ''
            ${pkgs.curl}/bin/curl -fSs http://p.r --data-binary @- \
            | ${pkgs.coreutils}/bin/tail --lines=1 \
            | ${pkgs.gnused}/bin/sed 's/\\<r\\>/krebsco.de/'
          '';
          "envs.sh paste" = pkgs.writers.writeDash "envs-host" ''
            ${pkgs.curl}/bin/curl -F "file=@-" https://envs.sh
          '';
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
          emojai = pkgs.writers.writeDash "emojai" ''
            ${pkgs.curl}/bin/curl https://www.emojai.app/api/generate -X POST -H 'Content-Type: application/json' --data-raw "$(${pkgs.jq}/bin/jq -sR '{emoji:.}')" | ${pkgs.jq}/bin/jq -r .result
          '';
          "gpt-3.5" = pkgs.writers.writeDash "gpt" ''
            ${niveumPackages.gpt35}/bin/gpt
          '';
          gpt-4 = pkgs.writers.writeDash "gpt" ''
            ${niveumPackages.gpt4}/bin/gpt
          '';
        };
      };
    in
    ''
      spawn-at-startup "${pkgs.ironbar}/bin/ironbar"
      spawn-at-startup "${pkgs.xwayland-satellite}/bin/xwayland-satellite"

      environment {
        DISPLAY ":0"
        ANKI_WAYLAND "1"
      }

      input {
          warp-mouse-to-focus
          focus-follows-mouse max-scroll-amount="0%"

          keyboard {
              repeat-rate 35
              repeat-delay 350
              track-layout "global"

              xkb {
                  layout "de"
                  variant "T3"
                  options "ctrl:nocaps,compose:caps,grp:ctrls_toggle"
              }
          }
          touchpad {
              click-method "clickfinger"
              tap
              dwt
              dwtp
          }
      }

      prefer-no-csd

      hotkey-overlay {
        skip-at-startup
      }

      layout {
          gaps 5

          default-column-width {
            proportion 0.5
          }

          preset-column-widths {
              proportion 0.33333
              proportion 0.5
              proportion 0.66667
          }

          focus-ring {
              width 2
          }

          shadow {
              // on
              softness 30
              spread 5
              offset x=0 y=5
              draw-behind-window true
              color "#00000070"
              // inactive-color "#00000054"
          }

          tab-indicator {
              // off
              hide-when-single-tab
              place-within-column
              gap 5
              width 4
              length total-proportion=1.0
              position "right"
              gaps-between-tabs 2
              corner-radius 8
              active-color "red"
              inactive-color "gray"
              urgent-color "blue"
              // active-gradient from="#80c8ff" to="#bbddff" angle=45
              // inactive-gradient from="#505050" to="#808080" angle=45 relative-to="workspace-view"
              // urgent-gradient from="#800" to="#a33" angle=45
          }

          border {
              off
          }
      }

      animations {
        // off
        workspace-switch {
            spring damping-ratio=1.0 stiffness=1000 epsilon=0.0001
        }

        window-open {
            duration-ms 150
            curve "ease-out-expo"
        }

        window-close {
            duration-ms 150
            curve "ease-out-quad"
        }

        horizontal-view-movement {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.0001
        }

        window-movement {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.0001
        }

        window-resize {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.0001
        }

        config-notification-open-close {
            spring damping-ratio=0.6 stiffness=1000 epsilon=0.001
        }

        screenshot-ui-open {
            duration-ms 200
            curve "ease-out-quad"
        }

        overview-open-close {
            spring damping-ratio=1.0 stiffness=800 epsilon=0.0001
        }
      }

      window-rule {
          geometry-corner-radius 0
          clip-to-geometry true
      }

      window-rule {
        match app-id="mpv"
        open-floating true
      }
      window-rule {
        match app-id="rofi"
        open-floating true
      }
      window-rule {
        match app-id=r#"firefox$"# title="^Picture-in-Picture$"
        open-floating true
        default-floating-position x=32 y=32 relative-to="bottom-left"
      }

      window-rule {
          match is-window-cast-target=true

          border {
              on
              width 3
              active-color "#f38ba8"
              inactive-color "#7d0d2d"
          }
      }

      binds {
          Mod+Shift+Slash { show-hotkey-overlay; }
          Mod+Return { spawn "${(defaultApplications pkgs).terminal}"; }
          Mod+D { spawn "${pkgs.wofi}/bin/wofi" "--show" "run"; }
          Mod+Shift+D { spawn "${niveumPackages.notemenu}/bin/notemenu"; }
          Mod+T { spawn "${(defaultApplications pkgs).fileManager}"; }
          Mod+Y { spawn "${(defaultApplications pkgs).browser}"; }
          Mod+P { spawn "${niveumPackages.passmenu}/bin/passmenu"; }
          Mod+U { spawn "${niveumPackages.unicodmenu}/bin/unicodmenu"; }

          Mod+B { spawn "${pkgs.ironbar}/bin/ironbar" "bar" "bar-1337" "toggle-visible"; }
          Mod+F12 { spawn "${klem}/bin/klem"; }

          Mod+Shift+Q { close-window; }

          XF86AudioRaiseVolume  allow-when-locked=true { spawn "${pkgs.pamixer}/bin/pamixer -i 5"; }
          XF86AudioLowerVolume  allow-when-locked=true { spawn "${pkgs.pamixer}/bin/pamixer -d 5"; }
          XF86AudioMute         allow-when-locked=true { spawn "${pkgs.pamixer}/bin/pamixer -t"; }

          XF86AudioPause allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl play-pause"; }
          XF86AudioPlay allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl play-pause"; }
          XF86AudioNext allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl next"; }
          XF86AudioPrev allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl previous"; }
          XF86AudioStop allow-when-locked=true { spawn "${pkgs.playerctl}/bin/playerctl stop"; }
          Print { spawn "flameshot gui"; }
          Mod+Shift+W { spawn "swaylock"; }

          Mod+Comma  { consume-or-expel-window-left; }
          Mod+Period { consume-or-expel-window-right; }
          Mod+W { toggle-column-tabbed-display; }
          Mod+A repeat=false { toggle-overview; }
          Mod+F { maximize-column; }
          Mod+C { center-column; }
          Mod+Minus { set-column-width "-25%"; }
          Mod+Plus { set-column-width "+25%"; }

          Mod+Ctrl+0 { spawn "niri" "msg" "action" "switch-layout" "0"; }
          Mod+Ctrl+1 { spawn "niri" "msg" "action" "switch-layout" "1"; }
          Mod+Ctrl+2 { spawn "niri" "msg" "action" "switch-layout" "2"; }
          Mod+Ctrl+3 { spawn "niri" "msg" "action" "switch-layout" "3"; }
          Mod+Ctrl+4 { spawn "niri" "msg" "action" "switch-layout" "4"; }
          Mod+Ctrl+5 { spawn "niri" "msg" "action" "switch-layout" "5"; }
          Mod+Ctrl+6 { spawn "niri" "msg" "action" "switch-layout" "6"; }
          Mod+Ctrl+7 { spawn "niri" "msg" "action" "switch-layout" "7"; }
          Mod+Ctrl+8 { spawn "niri" "msg" "action" "switch-layout" "8"; }
          Mod+Ctrl+9 { spawn "niri" "msg" "action" "switch-layout" "9"; }

          Mod+H     { focus-column-or-monitor-left; }
          Mod+J  { focus-window-or-workspace-down; }
          Mod+K  { focus-window-or-workspace-up; }
          Mod+L     { focus-column-or-monitor-right; }

          Mod+Shift+H     { move-column-left-or-to-monitor-left; }
          Mod+Shift+J  { move-window-down-or-to-workspace-down; }
          Mod+Shift+K  { move-window-up-or-to-workspace-up; }
          Mod+Shift+L     { move-column-right-or-to-monitor-right; }

          Mod+Ctrl+H { focus-monitor-left; }
          Mod+Ctrl+J { focus-monitor-down; }
          Mod+Ctrl+K { focus-monitor-up; }
          Mod+Ctrl+L { focus-monitor-right; }

          Mod+Shift+Ctrl+H     { move-column-to-monitor-left; }
          Mod+Shift+Ctrl+J     { move-column-to-workspace-down; }
          Mod+Shift+Ctrl+K     { move-column-to-workspace-up; }
          Mod+Shift+Ctrl+L     { move-column-to-monitor-right; }

          Mod+Shift+Alt+Ctrl+H     { move-workspace-to-monitor-left; }
          Mod+Shift+Alt+Ctrl+J     { move-workspace-down; }
          Mod+Shift+Alt+Ctrl+K     { move-workspace-up; }
          Mod+Shift+Alt+Ctrl+L     { move-workspace-to-monitor-right; }

          Mod+1 { focus-workspace 1; }
          Mod+2 { focus-workspace 2; }
          Mod+3 { focus-workspace 3; }
          Mod+4 { focus-workspace 4; }
          Mod+5 { focus-workspace 5; }
          Mod+6 { focus-workspace 6; }
          Mod+7 { focus-workspace 7; }
          Mod+8 { focus-workspace 8; }
          Mod+9 { focus-workspace 9; }
          Mod+0 { focus-workspace 10; }

          Mod+Shift+1 { move-window-to-workspace "1"; }
          Mod+Shift+2 { move-window-to-workspace "2"; }
          Mod+Shift+3 { move-window-to-workspace "3"; }
          Mod+Shift+4 { move-window-to-workspace "4"; }
          Mod+Shift+5 { move-window-to-workspace "5"; }
          Mod+Shift+6 { move-window-to-workspace "6"; }
          Mod+Shift+7 { move-window-to-workspace "7"; }
          Mod+Shift+8 { move-window-to-workspace "8"; }
          Mod+Shift+9 { move-window-to-workspace "9"; }
          Mod+Shift+0 { move-window-to-workspace "0"; }
      }
    '';
in
{
  system.activationScripts.niriConfig = {
    text = ''
      cp ${pkgs.writeText "config.kdl" niriConfig} ${config.users.users.me.home}/.config/niri/config.kdl
      chown ${config.users.users.me.name}:${config.users.users.me.group} ${config.users.users.me.home}/.config/niri/config.kdl
    '';
  };

  programs.niri.enable = true;
  services.displayManager.defaultSession = lib.mkForce "niri";
  home-manager.users.me = {
    xdg.configFile."ironbar/style.css".text = ''
      * {
        font-size: 8pt;
        font-family: "Gentium Plus", "BlexMono Nerd Font";
      }

      box, menubar, button {
        background-color: unset;
        box-shadow: none;
        background-image: none;
      }

      .clock, .upower, .volume {
        font-weight: unset;
      }

      tooltip * {
        font-family: "BlexMono Nerd Font";
        font-size: 7pt;
      }
    '';
    xdg.configFile."ironbar/config.json".source = (pkgs.formats.json { }).generate "ironbar.json" {
      name = "bar-1337";
      height = 12;
      layer = "top";
      position = "bottom";
      start = [ ];
      center = [
        {
          type = "tray";
          icon_size = 8;
        }
        { type = "clipboard"; }
        { type = "notifications"; }
      ];
      end = [
        {
          type = "upower";
          icon_size = 8;
          format = "{percentage}%";
        }
        {
          type = "label";
          tooltip = "{{df -h --output=size,used,avail,pcent,target}}";
          label = "\t{{5000:df -h / --output=avail | tail +2}}";
        }
        {
          type = "label";
          tooltip = "{{free -Lh --si |  awk '{for(i=1;i<=NF;i++){printf \"%s%s\", $i, (i%2? OFS: ORS)} if(NF%2) printf ORS}'}}";
          label = "󰍛\t{{500:free -h --si | awk 'NR==2{printf $3 \"\\n\"}'}}";
        }
        {
          type = "label";
          tooltip = "{{}}";
          on_click_left = "pamixer -t";
          on_scroll_up = "pamixer -i 1";
          on_scroll_down = "pamixer -d 1";
          label = "{{500:if $(pamixer --get-mute) = true; then echo ; else echo ; fi}}\t{{500:pamixer --get-volume}}%";
        }
        {
          type = "label";
          tooltip = "{{uptime}}";
          label = "\t{{500:uptime | sed 's/.*load average: \\([^ ]*\\);.*/\\1/' | tr ' ' '\n'}}";
        }
        {
          type = "label";
          tooltip = "{{khal list today today -d astro-test-3 }}";
          label = "";
        }
        {
          type = "label";
          tooltip = "{{curl wttr.in/?0 | ${pkgs.ansifilter}/bin/ansifilter}}";
          label = "󰔏";
        }
        {
          type = "label";
          name = "cal";
          tooltip = "{{cal}}";
          label = "{{500:date +'<U+F017>\t%Y-%m-%d (%W %a) %H:%M'}}";
        }
      ];
    };
    programs.alacritty.enable = true; # Super+T in the default setting (terminal)
    programs.swaylock.enable = true; # Super+Alt+L in the default setting (screen locker)
    services.swaync = {
      enable = true;
      settings = {
        notification-window-width = 300;
        control-center-width = 300;
        widgets = [
          "volume"
          "mpris"
          "title"
          "dnd"
          "notifications"
        ];
        widget-config = {
          title = {
            text = "ⲡⲧⲏⲣϥ̄";
            "clear-all-button" = true;
            "button-text" = "ⲧⲁⲩⲟⲟⲩ";
          };
          dnd.text = "ⲙ̄ⲡⲣ̄ϣⲧⲣ̄ⲧⲱⲣⲧ̄";
          label.text = "ⲧⲙⲏⲧⲉ";
        };
      };
    };
    services.swayidle.enable = true; # idle management daemon
    home.packages = with pkgs; [
      xdg-desktop-portal-gnome
      swaybg
    ];
  };
  services.gnome.gnome-keyring.enable = true; # secret service
  security.pam.services.swaylock = { };
}
