{ config, pkgs, ... }:
/* let vimConfig = import ../../dot/vim.nix { inherit config pkgs; };
in */ {
  imports = [
    ../../configs/shells.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";

  /*
  networking.wireless = {
    enable = true;
    networks.Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
  };
  environment.variables.EDITOR = "vim";
  environment.variables.HTOPRC = toString ../../dot/htop.nix;

  programs.tmux.enable = true;
  */

  environment.systemPackages = with pkgs; [
    git
    htop
/*    (vim_configurable.customize {
      name = "kvim";
      vimrcConfig = {
        customRC = vimConfig.vimrc;
        packages.kvim.start = vimConfig.startPackages;
      };
      })*/
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
    hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.bash;
  };
}
