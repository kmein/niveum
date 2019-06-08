{ pkgs, lib, config, ... }:
with lib;
let cfg = config.niveum.dropbox;
in
{
  options.niveum.dropbox = {
    enable = mkEnableOption "Dropbox";
    user = mkOption { type = types.attrs; };
  };

  config = mkIf cfg.enable {
    systemd.services.dropbox = {
      description = "Dropbox synchronisation service";
      after = [ "network-online.target" ];
      script = "${pkgs.dropbox}/bin/dropbox";
      serviceConfig = {
        Restart = "always";
        User = cfg.user.name;
      };
    };

    environment.systemPackages = [ pkgs.dropbox-cli ];
  };
}
