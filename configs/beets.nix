{ lib, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.beets ];
  home-manager.users.me.xdg.configFile = {
    "beets/config.yaml".source = (pkgs.formats.yaml {}).generate "config.yaml" {
      directory = "~/cloud/syncthing/music";
      library = "~/cloud/syncthing/common/music.db";
      plugins = lib.concatStringsSep " " [ "fetchart" "lastgenre" "lyrics" ];
    };
  };
}
