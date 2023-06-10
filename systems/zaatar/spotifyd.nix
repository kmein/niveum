{config, ...}: {
  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username_cmd = "cat $CREDENTIALS_DIRECTORY/username";
        password_cmd = "cat $CREDENTIALS_DIRECTORY/password";
        backend = "pulseaudio";
        bitrate = 320;
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
  };

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
