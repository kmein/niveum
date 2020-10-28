{ config, pkgs, lib, ... }:
let
  kmeinKeys = lib.strings.splitString "\n" (lib.strings.fileContents (pkgs.fetchurl {
    url = "https://github.com/kmein.keys";
    sha256 = "1b9gbpgihg7zc89ivsz0gs3najp0zg53rcknvzvkm0851fdzkryx";
  }));
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/wifi.nix>
    <niveum/configs/keyboard.nix>
    <niveum/modules/retiolum.nix>
    <niveum/modules/constants.nix>
    <niveum/configs/spotifyd.nix>
    <niveum/configs/spacetime.nix>
    {
      services.mpd = {
        enable = true;
        extraConfig = ''
          audio_output {
            type "pulse"
            name "Pulseaudio"
            server "127.0.0.1"
          }
        '';
      };

      hardware.pulseaudio.extraConfig = "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";

      services.ympd = {
        enable = true;
        webPort = 8080;
      };

      networking.firewall.extraCommands = ''
        ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport 8080 -s 192.168.0.0/16 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport 8080 -s 127.0.0.0/8 -j ACCEPT
        ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport 8080 -j DROP
      '';
    }
    {
      sound.enable = true;

      hardware.pulseaudio.enable = true;

      environment.systemPackages = [ pkgs.pavucontrol pkgs.pamixer ];
    }
  ];

  nix.nixPath = [ "/var/src" ];

  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
  };

  services.illum.enable = true;

  environment.systemPackages = with pkgs; [ git vim htop ];

  users.mutableUsers = false;
  users.users.kiosk = {
    isNormalUser = true;
    name = "kiosk";
    extraGroups = [ "audio" ];
    password = "";
    openssh.authorizedKeys.keys = kmeinKeys;
  };

  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    ];
  };

  services.xserver = {
    enable = true;
    enableCtrlAltBackspace = true;

    displayManager = {
      autoLogin = {
        enable = true;
        user = config.users.users.kiosk.name;
      };
      sessionCommands = ''
        ${pkgs.xorg.xset}/bin/xset -dpms
        ${pkgs.xorg.xset}/bin/xset s off
      '';
      session = [
        {
          manage = "desktop";
          name = "youtube";
          start = let startUrl = "https://youtube.com"; in ''
            export PATH=$PATH:${lib.makeBinPath [ pkgs.chromium pkgs.xorg.xrandr pkgs.gawk pkgs.gnused ]}
            SIZE="$(xrandr | awk '/\*\+/{print $1}' | sed s/x/,/)"

            chromium \
              --incognito --disable-translate \
              --no-first-run --no-message-box --noerrdialogs \
              --default-browser --no-default-browser-check \
              --start-maximized --window-position=0,0 --window-size="$SIZE" \
              --kiosk ${startUrl}
            waitPID=$!
          '';
        }
      ];
    };
  };

  services.openssh = {
    enable = true;
    ports = [ 22022 ];
    passwordAuthentication = false;
  };

  users.users.root.openssh.authorizedKeys.keys = kmeinKeys;

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "scardanelli";

  networking.retiolum = {
    ipv4 = "10.243.2.2";
    ipv6 = "42:0:3c46:4007:5bce:f1bc:606b:2b18";
  };

  system.stateVersion = "20.09";
}
