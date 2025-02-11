{pkgs, ...}: {
  hardware.bluetooth = {
    enable = true;
    settings.general = {
      enable = "Source,Sink,Media,Socket";
    };
  };

  environment.systemPackages = [ pkgs.bluetuith ];
}
