{ config, pkgs, ... }:
{
  users.mutableUsers = false;

  users.users.kfm = {
    name = "kfm";
    description = config.constants.user.name;
    home = "/home/kfm";
    createHome = true;
    group = "users";
    extraGroups = [ "wheel" "audio" "docker" ];
    hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.zsh;
  };
}
