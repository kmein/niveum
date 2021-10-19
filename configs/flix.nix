{ config, pkgs, ... }:
let
  flixLocation = "/media/flix";
  cacheLocation = "/var/cache/flix";
  indexFilename = "index";
  flixUser = "flix";
  flixGroup = "users";
  inherit (import <niveum/lib>) tmpfilesConfig;
in {
  fileSystems.${flixLocation} = {
    device = "prism.r:/export";
    fsType = "nfs";
    options = [
      "noauto"
      "noatime"
      "nodiratime"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
      "x-systemd.requires=tinc.retiolum.service"
      "x-systemd.requires=wpa_supplicant.service"
      "user"
      "_netdev"
    ];
  };

  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      path = cacheLocation;
      mode = "0750";
      user = flixUser;
      group = flixGroup;
    })
  ];

  systemd.services.flix-index = {
    description = "Flix indexing service";
    wants = [ "network-online.target" ];
    script = "cp ${flixLocation}/download/index ./${indexFilename}";
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      User = flixUser;
      Group = flixGroup;
      WorkingDirectory = cacheLocation;
    };
  };

  users.extraUsers.${flixUser} = {
    isSystemUser = true;
    createHome = true;
    home = cacheLocation;
    extraGroups = [ flixGroup ];
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "mpv-simpsons" ''
      set -efu
      cd "${flixLocation}/download"
      [ -f "${cacheLocation}/${indexFilename}" ] || exit 1

      cat "${cacheLocation}/${indexFilename}" \
        | ${pkgs.gnugrep}/bin/grep -i 'simpsons.*mkv' \
        | shuf \
        | ${pkgs.findutils}/bin/xargs -d '\n' ${pkgs.mpv}/bin/mpv
    '')
    (pkgs.writers.writeDashBin "flixmenu" ''
      set -efu
      cd "${flixLocation}/download"

      [ -f "${cacheLocation}/${indexFilename}" ] || exit 1

      ${pkgs.dmenu}/bin/dmenu -i -p flix -l 5 "$@" < ${cacheLocation}/${indexFilename} \
        | ${pkgs.findutils}/bin/xargs -I '{}' ${pkgs.utillinux}/bin/setsid ${pkgs.xdg_utils}/bin/xdg-open '{}'
    '')
  ];

}
