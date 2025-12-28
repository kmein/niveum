{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../configs/spacetime.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/nix.nix
    ../../configs/admin-essentials.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/tabula-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/tabula-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
  };

  services.libinput.enable = true;

  users.users.xenos = {
    name = "xenos";
    password = "xenos";
    isNormalUser = true;
    extraGroups = [ "networkmanager" ];
  };

  services.displayManager = {
    autoLogin = {
      enable = true;
      user = "xenos";
    };
  };

  services.xserver = {
    enable = true;
    desktopManager.lxqt.enable = true;
  };

  environment.systemPackages = [
    pkgs.libreoffice
    pkgs.gimp
    pkgs.inkscape
    pkgs.firefox
    pkgs.pidgin
    pkgs.git
    pkgs.vim
  ];

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s4.useDHCP = true;
      wlp2s0.useDHCP = true;
    };
    retiolum = pkgs.lib.niveum.retiolumAddresses.tabula;
    hostName = "tabula";
  };

  networking.networkmanager.enable = true;

  system.stateVersion = "21.11";
}
