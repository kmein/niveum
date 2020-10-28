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
    <niveum/configs/spacetime.nix>
    <niveum/configs/mpd.nix>
    <niveum/configs/sshd.nix>
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
        openssh.authorizedKeys.keys = kmeinKeys;
      };
      services.cage = {
        enable = true;
        user = config.users.extraUsers.kiosk.name;
        program = let startUrl = "https://youtube.com"; in ''
          ${pkgs.chromium}/bin/chromium \
            --incognito --disable-translate \
            --no-first-run --no-message-box --noerrdialogs \
            --default-browser --no-default-browser-check \
            --start-maximized --kiosk ${startUrl}
        '';
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
