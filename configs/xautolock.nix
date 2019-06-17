{ config, pkgs, lib, ... }:
let
  xlockModes = lib.concatStringsSep "\\n" [
    # "braid"
    "galaxy"
    "lightning"
    # "matrix"
    "pyro2"
    "space"
  ];
  my-xlock = pkgs.unstable.writers.writeDashBin "xlock" ''
    MODE=$(printf "${xlockModes}" | shuf -n 1)

    ${pkgs.xlockmore}/bin/xlock \
      -saturation 0.4 \
      -erasemode no_fade \
      +description \
      -showdate \
      -username " " \
      -password " " \
      -info " " \
      -validate "..." \
      -invalid "Computer says no." \
      -mode "$MODE"
  '';
in
{
  services.xserver.xautolock = rec {
    enable = true;
    killer = "${pkgs.systemd}/bin/systemctl suspend";
    locker = "${my-xlock}/bin/xlock";
    nowlocker = locker;
    enableNotifier = true;
    notifier = ''${pkgs.libnotify}/bin/notify-send -u normal -a xautolock "Locking" "in 10 seconds."'';
  };
}
