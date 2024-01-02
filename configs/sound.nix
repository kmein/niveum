{pkgs, ...}: {
  sound.enable = true;

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

  environment.etc."pipewire/pipewire-pulse.conf.d/50-network-party.conf".text = ''
    context.exec = [
      { path = "pactl" args = "load-module module-native-protocol-tcp" }
      { path = "pactl" args = "load-module module-zeroconf-discover" }
      { path = "pactl" args = "load-module module-zeroconf-publish" }
    ]
  '';

  environment.systemPackages = [
    pkgs.pavucontrol
    pkgs.ncpamixer
    pkgs.pamixer
    pkgs.pulsemixer
    pkgs.pulseaudio # for pactl
  ];
}
