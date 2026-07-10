{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./hdmi.nix
    ../../configs/default.nix
    ../../configs/tlp.nix
    ../../configs/networkmanager.nix
    ../../configs/power-action.nix
  ];

  age.secrets = {
    syncthing-cert.file = ../../secrets/manakish-syncthing-cert.age;
    syncthing-key.file = ../../secrets/manakish-syncthing-key.age;
    wireguard-aether-key.file = ../../secrets/manakish-wireguard-aether-key.age;
    wireguard-aether-psk.file = ../../secrets/manakish-wireguard-aether-psk.age;
  };

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "green";
  };

  # Sandy Bridge (Gen6) HD Graphics 3000 has no hardware Vulkan driver
  # (anv is Gen9+, hasvk is Gen7-8), so wgpu falls back to software lavapipe,
  # which lacks SHADER_F16_IN_F32 and panics compiling iced_wgpu's f16 quad
  # shader (crashes ashell). Force wgpu onto the hardware GL backend (crocus),
  # where naga lowers the f16 packing to core unpackHalf2x16 and it just works.
  environment.sessionVariables.WGPU_BACKEND = "gl";

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s25.useDHCP = true;
      wlp3s0.useDHCP = true;
      wwp0s20u4i6.useDHCP = true;
    };
    wireless.interfaces = [ "wlp3s0" ];
    retiolum = pkgs.lib.niveum.retiolumAddresses.manakish;
    hostName = "manakish";
  };

  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  system.stateVersion = "20.09"; # Did you read the comment?
}
