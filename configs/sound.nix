{pkgs, ...}: {
  sound.enable = true;

  # realtime audio
  security.rtkit.enable = true;

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

  hardware.pulseaudio = {
    enable = false;
    package = pkgs.pulseaudioFull;
    # copy server:/run/pulse/.config/pulse/cookie to client:~/.config/pulse/cookie to authenticate a client machine
    zeroconf.discovery.enable = true;
    extraConfig = ''
      load-module ${
        toString [
          "module-tunnel-sink-new"
          "server=zaatar.r"
          "sink_name=zaatar"
          "channels=2"
          "rate=44100"
        ]
      }
    '';
  };

  users.users.me.extraGroups = ["pipewire" "audio"];

  environment.systemPackages = [
    pkgs.pavucontrol
    pkgs.ncpamixer
    pkgs.pamixer
    pkgs.pulsemixer
    pkgs.pulseaudio # for pactl
  ];
}
