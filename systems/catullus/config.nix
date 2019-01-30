{ config, pkgs, ... }:
let vimConfig = import ../../dot/vim.nix { inherit config pkgs; };
in {
  imports = [
    ../configs/users.nix
    ../configs/htop.nix
    ../configs/shells.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";

  network.wireless = {
    enable = true;
    networks.Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
  };

  services.openssh.enable = true;

  programs.tmux.enable = true;
  environment.systemPackages = with pkgs; [
    git
    (vim_configurable.customize {
      name = "kvim";
      vimrcConfig = {
        customRC = vimConfig.vimrc;
        packages.kvim.start = vimConfig.startPackages;
      };
    })
  ];
}
