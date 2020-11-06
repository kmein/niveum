{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/wifi.nix>
    <niveum/configs/keyboard.nix>
    <niveum/modules/retiolum.nix>
    <niveum/configs/tuner.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/mpd.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/version.nix>
    <niveum/configs/spotifyd.nix>
    {
      sound.enable = true;
    }
    {
      services.illum.enable = true;
    }
    {
      users.extraUsers.kiosk = {
        isNormalUser = true;
        password = "";
        extraGroups = [ "audio" ];
      };
      services.cage = {
        enable = true;
        user = config.users.extraUsers.kiosk.name;
        extraArguments = [ "-s" ]; # allow vt switching
        program =
        let startUrl = "https://youtube.com";
        in pkgs.writers.writeDash "kiosk-browser" ''
          while true; do
            ${pkgs.chromium}/bin/chromium \
              --no-first-run --no-message-box --noerrdialogs \
              --default-browser --no-default-browser-check \
              --start-maximized --kiosk ${startUrl}
            sleep 0.5
          done
        '';
      };
      systemd.services.cage-tty1.environment.XKB_DEFAULT_LAYOUT = "de";
      programs.chromium = {
        enable = true;
        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        ];
      };
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

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zaatar";

  networking.retiolum = {
    ipv4 = "10.243.2.2";
    ipv6 = "42:0:3c46:4007:5bce:f1bc:606b:2b18";
  };

  system.stateVersion = "20.09";
}
