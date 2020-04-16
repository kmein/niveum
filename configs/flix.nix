{ config, pkgs, ... }:
let
  flixLocation = "/media/flix";
  cacheLocation = "/var/cache/flix";
in {
  imports = [ <stockholm/krebs/3modules/permown.nix> ];

  fileSystems.${flixLocation} = {
    device = "prism.r:/export";
    fsType = "nfs";
  };

  krebs.permown.${cacheLocation} = {
    owner = config.users.users.me.name;
    group = "users";
    umask = "0000";
  };

  systemd.services.flix-index = {
    description = "Flix indexing service";
    wants = [ "network-online.target" ];
    script = ''
      flix_cache="${cacheLocation}/flix_index"

      find ${flixLocation}/download/sorted -type f > "$flix_cache"
    '';
    startAt = "hourly";
    serviceConfig.Type = "oneshot";
  };

  environment.systemPackages = [
    (pkgs.writeDashBin "flixmenu" ''
      set -efu
      flix_cache="${cacheLocation}/flix_index"

      [ -f "$flix_cache" ] || exit 1

      ${pkgs.dmenu}/bin/dmenu -i -p flix -l 5 "$@" < "$flix_cache" \
        | ${pkgs.findutils}/bin/xargs -I '{}' ${pkgs.utillinux}/bin/setsid ${pkgs.xdg_utils}/bin/xdg-open '{}'
    '')
  ];

}
