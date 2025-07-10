{
  config,
  niveumPackages,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  inherit (import ../../lib) retiolumAddresses defaultApplications;
  # TODO wrap obsidian: obsidian --no-sandbox --ozone-platform=wayland --ozone-platform-hint=auto --enable-features=UseOzonePlatform,WaylandWindowDecorations %U
in
{
  imports = [
    ../kibbeh/hardware-configuration.nix
    ../../configs/tlp.nix
    ../../configs/default.nix
    ../../configs/networkmanager.nix
    ../../configs/power-action.nix
    {
      programs.niri.enable = true;
      services.displayManager.defaultSession = lib.mkForce "niri";
      home-manager.users.me = {
        imports = [ inputs.centerpiece.hmModules."x86_64-linux".default ];
        programs.centerpiece = {
          enable = true;
          config.plugin = {
            clock.enable = true;
            resource_monitor_battery.enable = true;
            resource_monitor_cpu.enable = true;
            resource_monitor_disks.enable = true;
            resource_monitor_memory.enable = true;
            system.enable = true;
            wifi.enable = true;
          };
        };

        programs.alacritty.enable = true; # Super+T in the default setting (terminal)
        programs.fuzzel.enable = true; # Super+D in the default setting (app launcher)
        programs.swaylock.enable = true; # Super+Alt+L in the default setting (screen locker)
        programs.waybar.enable = true; # launch on startup in the default setting (bar)
        services.mako.enable = true; # notification daemon
        services.swayidle.enable = true; # idle management daemon
        home.packages = with pkgs; [
          xdg-desktop-portal-gnome
          swaybg
        ];
        home.file.".config/niri/config.kdl".text =
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
            spawn-at-startup "waybar"

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
                        options "ctrl:nocaps,compose:caps"
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
                Mod+D { spawn "fuzzel"; }
                Mod+Shift+D { spawn "${niveumPackages.notemenu}/bin/notemenu"; }
                Mod+T { spawn "${(defaultApplications pkgs).fileManager}"; }
                Mod+Y { spawn "${(defaultApplications pkgs).browser}"; }
                Mod+P { spawn "rofi-pass"; }
                Mod+U { spawn "${niveumPackages.unicodmenu}/bin/unicodmenu"; }

                Mod+B { spawn "pkill -SIGUSR1 waybar"; }
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
      };
      services.gnome.gnome-keyring.enable = true; # secret service
      security.pam.services.swaylock = { };
    }
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  nix.settings = {
    cores = 1;
    max-jobs = 2;
  };

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/kabsa-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/kabsa-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/kabsa-syncthing-cert.age;
    syncthing-key.file = ../../secrets/kabsa-syncthing-key.age;
    wireguard-aether-key.file = ../../secrets/kabsa-wireguard-aether-key.age;
    wireguard-aether-psk.file = ../../secrets/kabsa-wireguard-aether-psk.age;
  };

  networking.wg-quick.interfaces.aether.address = [ "192.168.178.203/24" ];

  environment.systemPackages = [ pkgs.zeroad ];

  networking = {
    hostName = "kabsa";
    wireless.interfaces = [ "wlp3s0" ];
    retiolum = retiolumAddresses.kabsa;
  };

  system.stateVersion = "23.11";
}
