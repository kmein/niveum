{ pkgs, ... }:
{
  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };

  environment.systemPackages = [ pkgs.blueman ];

  home-manager.users.me = {
    services.blueman-applet.enable = true;
  };
}
