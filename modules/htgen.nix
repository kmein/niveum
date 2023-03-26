{
  config,
  lib,
  pkgs,
  ...
}: let
  htgen = pkgs.callPackage ../packages/htgen.nix {};
in {
  options.services.htgen = lib.mkOption {
    default = {};
    type = lib.types.attrsOf (lib.types.submodule ({config, ...}: {
      options = {
        enable = lib.mkEnableOption "htgen-${config._module.args.name}";
        port = lib.mkOption {
          type = lib.types.int;
        };
        script = lib.mkOption {
          type = lib.types.str;
        };
      };
    }));
  };
  config = {
    systemd.services =
      lib.mapAttrs' (
        name: cfg:
          lib.nameValuePair "htgen-${name}" {
            wantedBy = ["multi-user.target"];
            after = ["network.target"];
            environment = {
              HOME = "/var/lib/htgen-${name}";
              HTGEN_PORT = toString cfg.port;
              HTGEN_SCRIPT = cfg.script;
            };
            serviceConfig = {
              SyslogIdentifier = "htgen-${name}";
              DynamicUser = true;
              StateDirectory = "htgen-${name}";
              PrivateTmp = true;
              Restart = "always";
              ExecStart = "${htgen}/bin/htgen --serve";
            };
          }
      )
      config.services.htgen;
  };
}
