{
  pkgs,
  lib,
  ...
}: {
  hardware.bluetooth = {
    enable = true;
    settings.General.Enable =
      lib.concatStringsSep "," ["Source" "Sink" "Media" "Socket"];
  };

  services.blueman.enable = true;

  # environment.systemPackages = [pkgs.blueman];

  home-manager.users.me = {services.blueman-applet.enable = true;};
}
