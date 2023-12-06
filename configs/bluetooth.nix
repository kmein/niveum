{pkgs, ...}: {
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez;
    settings.general = {
      enable = "Source,Sink,Media,Socket";
    };
  };

  services.blueman.enable = true;

  # environment.systemPackages = [pkgs.blueman];

  home-manager.users.me = {services.blueman-applet.enable = true;};
}
