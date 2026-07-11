{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./cuda.nix
    ../../configs/applicative.nix
    ./hardware-configuration.nix
    ../../configs/networkmanager.nix
    ../../configs/default.nix
    ../../configs/ccc.nix
    ../../configs/gaming.nix
    # ../../configs/gnome.nix
  ];

  niveum = {
    batteryName = "BAT1";
    wirelessInterface = "wlp3s0";
    promptColours.success = "blue";
  };

  services.illum.enable = true;

  # ELAN touchscreen is haunted
  niphas.niri.settings.input.touch.off = { };

  # Synaptics 06cb:009a "Metallica" fingerprint reader — unsupported by mainline
  # libfprint, driven via ahbnr/nixos-06cb-009a-fingerprint-sensor
  services."06cb-009a-fingerprint-sensor" = {
    enable = true;
    backend = "libfprint-tod";
    calib-data-file = ./calib-data.bin;
  };

  # fprintd enrollment needs polkit "auth_self", but this sway session runs no
  # polkit authentication agent, so `fprintd-enroll` dies with PermissionDenied.
  # Grant enroll/delete to the physically-present (active, local) user. Verify
  # already defaults to "yes", so sudo/login fingerprint auth needs no rule.
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id == "net.reactivated.fprint.device.enroll" &&
          subject.active && subject.local) {
        return polkit.Result.YES;
      }
    });
  '';

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  age.secrets = {
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/fatteh-syncthing-cert.age;
    syncthing-key.file = ../../secrets/fatteh-syncthing-key.age;
    wireguard-aether-key.file = ../../secrets/fatteh-wireguard-aether-key.age;
    wireguard-aether-psk.file = ../../secrets/fatteh-wireguard-aether-psk.age;
  };

  networking.hostName = "fatteh";
  networking.retiolum = pkgs.lib.niveum.retiolumAddresses.fatteh;

  system.stateVersion = "23.11";
}
