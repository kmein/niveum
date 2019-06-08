{ pkgs, config, lib, ... }:
with lib;
let cfg = config.niveum.seafile;
in {
  options.niveum.seafile = {
    enable = mkEnableOption "Seafile";
    user = mkOption { type = types.attrs; };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.seafile = {
      description = "Seafile synchronisation service";
      after = [ "network-online.target" ];
      script = "${pkgs.seafile-client}/bin/seafile-applet";
      serviceConfig = {
        Restart = "always";
        User = cfg.user.name;
      };
    };

    environment.systemPackages = [ pkgs.seafile-client pkgs.seafile-shared ];
  };
}
