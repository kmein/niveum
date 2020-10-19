{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.watson ];

  system.activationScripts.watsonFiles = ''
    install -d ${config.users.users.me.home}/.config/watson/
    ln -sf ${config.users.users.me.home}/cloud/Dropbox/watson-frames.json ${config.users.users.me.home}/.config/watson/frames
  '';
}
