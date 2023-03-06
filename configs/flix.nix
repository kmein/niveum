{
  config,
  pkgs,
  ...
}: let
  flixLocation = "/media/flix";
  flixLocationNew = "/media/flix-new";
  cacheLocation = "/var/cache/flix";
  indexFilename = "index";
  indexFilenameNew = "index-new";
  flixUser = "flix";
  flixGroup = "users";
  inherit (import ../lib) tmpfilesConfig;
in {
  fileSystems.${flixLocation} = {
    device = "prism.r:/export/download";
    fsType = "nfs";
    options = [
      "noauto"
      "noatime"
      "nodiratime"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
      "x-systemd.requires=tinc.retiolum.service"
      "user"
      "_netdev"
    ];
  };

  fileSystems.${flixLocationNew} = {
    device = "//yellow.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
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
    wants = ["network-online.target"];
    script = ''
      cp ${flixLocation}/index ./${indexFilename}
      cp ${flixLocationNew}/index ./${indexFilenameNew}
    '';
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
    group = flixGroup;
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
      cd "${flixLocation}"

      cat ${cacheLocation}/${indexFilename} ${cacheLocation}/${indexFilenameNew} \
        | ${pkgs.dmenu}/bin/dmenu -i -p flix -l 5 "$@" \
        | ${pkgs.findutils}/bin/xargs -I '{}' ${pkgs.util-linux}/bin/setsid ${pkgs.xdg-utils}/bin/xdg-open '{}'
    '')
  ];
}
