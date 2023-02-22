{
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    ../../configs/spacetime.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/nix.nix
  ];

  age.secrets = {
    retiolum-rsa.file = ../../secrets/tabula-retiolum-privateKey-rsa.age;
    retiolum-ed25519.file = ../../secrets/tabula-retiolum-privateKey-rsa.age;
  };

  services.xserver = {
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
    desktopManager.lxqt.enable = true;
    displayManager = {
      autoLogin = {
        enable = true;
        user = "xenos";
      };
    };
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
    retiolum = retiolumAddresses.tabula;
    hostName = "tabula";
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  networking.networkmanager.enable = true;

  system.stateVersion = "21.11";
}
