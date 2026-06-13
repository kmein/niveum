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
    ../../configs/sshd.nix
  ];

  services.libinput.enable = true;

  users.users.lndw = {
    name = "lndw";
    password = "lndw";
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  services.displayManager = {
    autoLogin = {
      enable = true;
      user = "lndw";
    };
  };

  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
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
    hostName = "tabula";
  };

  networking.networkmanager.enable = true;

  system.stateVersion = "21.11";
}
