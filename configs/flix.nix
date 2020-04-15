{ config, pkgs, ... }:
let flixLocation = "/media/flix";
in {
  fileSystems.${flixLocation} = {
    device = "prism.r:/export";
    fsType = "nfs";
  };

  systemd.user.services.flix-index = {
    description = "Flix indexing service";
    after = [ "network.target" ];
    wantedBy = [ "default.target" ];
    script = ''
      cachedir="${config.users.users.me.home}/.cache"
      flix_cache="$cachedir/flix_index"

      [ -d "$cachedir" ] || mkdir -p "$cachedir"

      find ${flixLocation} -type f > "$flix_cache"
    '';
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      User = config.users.users.me.name;
    };
  };

  environment.systemPackages = [
    (pkgs.writeDashBin "flixmenu" ''
      set -efu
      cachedir="${config.users.users.me.home}/.cache"
      flix_cache="$cachedir/flix_index"

      [ -f "$flix_cache" ] || exit 1

      ${pkgs.dmenu}/bin/dmenu -i -p flix -l 5 < "$flix_cache" \
        | ${pkgs.findutils}/bin/xargs -I '{}' ${pkgs.xdg_utils}/bin/xdg-open '{}'
    '')
  ];

}
