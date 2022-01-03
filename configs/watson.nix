{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.watson ];

  home-manager.users.me.xdg.configFile."watson/config".text = ''
    [options]
    confirm_new_project = true
    confirm_new_tag = true
    date_format = %Y-%m-%d
    log_current = true
    report_current = true
  '';

  system.activationScripts.watson-home = ''
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Documents/watson/frames ${config.users.users.me.home}/.config/watson/frames
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Documents/watson/state ${config.users.users.me.home}/.config/watson/state
  '';
}
