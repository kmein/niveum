{
  pkgs,
  lib,
  ...
}:
{
  # Also set by home-assistant.nix; both say true, so the definitions merge.
  # Kept here so the daemon stands on its own once HA goes.
  hardware.bluetooth.enable = true;

  systemd.services.kombo-wake = {
    description = "Wake the Teufel Kombo 42 when the teufel Chromecast plays";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [
      "network-online.target"
      "bluetooth.target"
    ];

    environment = {
      # The cast device that feeds the Kombo's aux input. Addressed directly:
      # pychromecast's mDNS browser finds nothing from zaatar even though
      # avahi resolves the name, and the address is static anyway.
      KOMBO_CAST_HOST = "192.168.0.194";
      KOMBO_CAST_UUID = "29c3c6e3-928c-b14f-43ec-3009f91f421b";
      # The Bot sitting on the receiver's power button.
      KOMBO_BOT_MAC = "C9:36:35:30:41:08";
      KOMBO_STATE = "/var/lib/kombo-wake/state.json";
    };

    serviceConfig = {
      ExecStart = lib.getExe pkgs.kombo-wake;
      # Talks to bluetoothd over the system bus, so no DynamicUser.
      StateDirectory = "kombo-wake";
      Restart = "always";
      RestartSec = 10;
    };
  };
}
