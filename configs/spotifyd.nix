{ pkgs, lib, ... }:
let
  inherit (lib.strings) fileContents;
in {
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/audio/spotifyd.nix
  systemd.user.services.spotifyd = let
    spotifyd = pkgs.spotifyd.override {
      withMpris = true;
      withPulseAudio = true;
      inherit (pkgs) libpulseaudio dbus;
    };
    spotifydConf = pkgs.writeText "spotifyd.conf" (lib.generators.toINI { } {
      global = {
        username = fileContents <secrets/spotify/username>;
        password = fileContents <secrets/spotify/password>;
        backend = "pulseaudio";
      };
    });
  in {
    wantedBy = [ "default.target" ];
    after = [ "network-online.target" "sound.target" ];
    description = "spotifyd, a Spotify playing daemon";
    serviceConfig = {
      ExecStart =
        "${spotifyd}/bin/spotifyd --no-daemon --config-path ${spotifydConf}";
      Restart = "always";
      RestartSec = 12;
    };
  };
}
