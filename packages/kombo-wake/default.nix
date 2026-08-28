{
  lib,
  writers,
  python3,
}:

# The Teufel Kombo 42's KB 42 receiver has no wake path of its own: no CEC, no
# signal sensing, no trigger input, and a mains cut returns it to standby with
# the volume reset. The only way in is the front power button, which a
# SwitchBot Bot presses over BLE. zaatar is the only machine in radio range of
# that Bot, so the daemon runs there.
#
# Python rather than shell: pychromecast and pySwitchbot between them already
# speak both protocols, and the SwitchBot half would otherwise mean
# hand-rolling the WoHand GATT commands.
(writers.writePython3Bin "kombo-wake"
  {
    libraries = [
      python3.pkgs.pyswitchbot
      python3.pkgs.pychromecast
    ];
    flakeIgnore = [ "E501" ];
  }
  ./kombo-wake.py
).overrideAttrs
  (old: {
    meta = (old.meta or { }) // {
      description = "Wake the Teufel Kombo 42 when a Chromecast starts playing";
      mainProgram = "kombo-wake";
      # BlueZ over D-Bus, so Linux only.
      platforms = lib.platforms.linux;
    };
  })
