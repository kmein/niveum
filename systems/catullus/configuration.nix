{ config, pkgs, ... }:
{
  imports = [
    ../../configs/shells.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";

  time.timeZone = "Europe/Berlin";

  networking.wireless = {
    enable = true;
    networks.Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
  };
  environment.variables.EDITOR = "vim";
  environment.variables.HTOPRC = toString ../../dot/htoprc;
  environment.variables.TERM = "linux";

  programs.tmux.enable = true;

  environment.systemPackages = with pkgs; [
    git
    htop
    vim
  ];

  services.openssh.enable = true;

  users.users.kfm = {
    name = "kfm";
    description = "Kierán Meinhardt";
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };
}
