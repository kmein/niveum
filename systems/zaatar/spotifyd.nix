{config, ...}: {
  nixpkgs.config.packageOverrides = pkgs: {
    # mpris is a dbus service for controlling all music players with e.g. playerctl
    # I do not need this, because I only interact with the service via Spotify Connect
    # otherẃise it will pull in DBus which fails without X11
    spotifyd = pkgs.spotifyd.overrideAttrs {
      withMpris = false;
      withKeyring = false;
    };
  };

  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username_cmd = "cat $CREDENTIALS_DIRECTORY/username";
        password_cmd = "cat $CREDENTIALS_DIRECTORY/password";
        bitrate = 320;
        use_mpris = false;
        device_type = "s_t_b"; # set-top box
        device_name = config.networking.hostName;
      };
    };
  };

  systemd.services.spotifyd = {
    serviceConfig.LoadCredential = [
      "username:${config.age.secrets.spotify-username.path}"
      "password:${config.age.secrets.spotify-password.path}"
    ];
    serviceConfig.RuntimeMaxSec = "${toString (5 * 60 * 60)}s";
    serviceConfig.SupplementaryGroups = ["pipewire"];
  };

  networking.firewall.allowedTCPPorts = [4713];

  age.secrets = {
    spotify-username.file = ../../secrets/spotify-username.age;
    spotify-password.file = ../../secrets/spotify-password.age;
  };

  # ref https://github.com/NixOS/nixpkgs/issues/71362#issuecomment-753461502
  hardware.pulseaudio.extraConfig = ''
    unload-module module-native-protocol-unix
    load-module module-native-protocol-unix auth-anonymous=1
  '';
}
