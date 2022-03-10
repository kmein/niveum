{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.spotifyd;
  toml = pkgs.formats.toml {};
  spotifydConf =
    if cfg.settings != {}
    then toml.generate "spotify.conf" cfg.settings
    else pkgs.writeText "spotifyd.conf" cfg.config;
in {
  options = {
    services.spotifyd = {
      enable = mkEnableOption "spotifyd, a Spotify playing daemon";

      config = mkOption {
        default = "";
        type = types.lines;
        description = ''
          (Deprecated) Configuration for Spotifyd. For syntax and directives, see
          <link xlink:href="https://github.com/Spotifyd/spotifyd#Configuration"/>.
        '';
      };

      settings = mkOption {
        default = {};
        type = toml.type;
        description = ''
          Configuration for Spotifyd. For syntax and directives, see
          <link xlink:href="https://github.com/Spotifyd/spotifyd#Configuration"/>.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (cfg.config == "" && cfg.settings != {}) || (cfg.config != "" && cfg.settings == {});
        message = "Using the stringly typed .config attribute is discouraged. Use the TOML typed .settings attribute instead.";
      }
    ];

    systemd.services.spotifyd = {
      wantedBy = ["multi-user.target"];
      after = ["network-online.target" "sound.target"];
      description = "spotifyd, a Spotify playing daemon";
      environment.SHELL = "/bin/sh";
      serviceConfig = {
        ExecStart = "${pkgs.spotifyd}/bin/spotifyd --no-daemon --cache-path /var/cache/spotifyd --config-path ${spotifydConf}";
        Restart = "always";
        RestartSec = 12;
        DynamicUser = true;
        CacheDirectory = "spotifyd";
        SupplementaryGroups = ["audio"];
      };
    };
  };

  meta.maintainers = [maintainers.anderslundstedt];
}
