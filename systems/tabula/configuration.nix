{
  config,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/spacetime.nix>
    <niveum/modules/retiolum.nix>
    <niveum/configs/sshd.nix>
  ];

  nix.nixPath = ["/var/src"];

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
