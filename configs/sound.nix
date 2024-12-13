{pkgs, ...}: {
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    jack.enable = true;
  };

  systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];

  services.avahi = {
    enable = true;
    publish.enable = true;
    publish.userServices = true;
  };

  environment.systemPackages = [
    pkgs.pavucontrol
    pkgs.ncpamixer
    pkgs.pamixer
    pkgs.pulsemixer
    pkgs.pulseaudio # for pactl
  ];
}
