{
  inputs,
  pkgs,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    ../../configs/spacetime.nix
    ../../configs/sshd.nix
    ../../configs/retiolum.nix
    ../../configs/nix.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = inputs.secrets + "/tahina-retiolum-privateKey-rsa.age";
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = inputs.secrets + "/tahina-retiolum-privateKey-ed25519.age";
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
  };

  console.keyMap = "de";
  i18n.defaultLocale = "de_DE.UTF-8";
  services.xserver = {
    layout = "de";
    libinput.enable = true;
  };

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
    pkgs.git
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
