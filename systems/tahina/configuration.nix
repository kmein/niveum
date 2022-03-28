{
  config,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/battery.nix>
    <niveum/configs/spacetime.nix>
    <niveum/modules/retiolum.nix>
    <niveum/configs/sshd.nix>
    {
      console.keyMap = "de";
      i18n.defaultLocale = "de_DE.UTF-8";
      services.xserver = {
        layout = "de";
        libinput.enable = true;
      };
    }
    {
      nix.nixPath = ["/var/src"];
    }
  ];

  users.users.xenos = {
    name = "xenos";
    password = "xenos";
    isNormalUser = true;
    extraGroups = ["networkmanager"];
  };

  services.xserver = {
    enable = true;
    desktopManager.pantheon.enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        greeters.pantheon.enable = true;
      };
      autoLogin = {
        enable = true;
        user = "xenos";
      };
    };
  };
  boot.plymouth.enable = true;

  environment.systemPackages = [
    pkgs.libreoffice
    pkgs.gimp
    pkgs.inkscape
    pkgs.firefox
    pkgs.audacity
    pkgs.pidgin
  ];

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s25.useDHCP = true;
      wlo1.useDHCP = true;
    };
    retiolum = retiolumAddresses.tahina;
    hostName = "tahina";
  };

  system.stateVersion = "21.11";
}
