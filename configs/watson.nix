{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.watson ];

  system.activationScripts.watson-home = ''
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Documents/watson ${config.users.users.me.home}/.config/watson
  '';
}
