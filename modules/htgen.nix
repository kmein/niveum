{
  config,
  lib,
  pkgs,
  ...
}:

let
  optionalAttr = name: value: if name != null then { ${name} = value; } else { };

  cfg = config.services.htgen;

  out = {
    options.services.htgen = api;
    config = imp;
  };

  htgen = pkgs.callPackage ../packages/htgen.nix { };

  api =
    with lib;
    mkOption {
      default = { };
      type = types.attrsOf (
        types.submodule (
          { config, ... }:
          {
            options = {
              enable = mkEnableOption "services.htgen-${config._module.args.name}";

              name = mkOption {
                type = types.str;
                default = config._module.args.name;
              };

              package = mkOption {
                default = htgen;
                type = types.package;
              };

              port = mkOption {
                type = types.port;
              };

              script = mkOption {
                type = types.nullOr types.str;
                default = null;
              };

              scriptFile = mkOption {
                type = types.nullOr (types.either types.package types.pathname);
                default = null;
              };

              user = mkOption {
                default = {
                  name = "htgen-${config.name}";
                  home = "/var/lib/htgen-${config.name}";
                };
                defaultText = {
                  name = "htgen-‹name›";
                  home = "/var/lib/htgen-‹name›";
                };
              };
            };
          }
        )
      );
    };
  imp = {

    systemd.services = lib.mapAttrs' (
      name: htgen:
      lib.nameValuePair "htgen-${name}" {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        environment = {
          HTGEN_PORT = toString htgen.port;
        }
        // optionalAttr "HTGEN_SCRIPT" htgen.script
        // optionalAttr "HTGEN_SCRIPT_FILE" htgen.scriptFile;
        serviceConfig = {
          SyslogIdentifier = "htgen";
          User = htgen.user.name;
          PrivateTmp = true;
          Restart = "always";
          ExecStart = "${htgen.package}/bin/htgen --serve";
        };
      }
    ) cfg;

    users.users = lib.mapAttrs' (
      name: htgen:
      lib.nameValuePair htgen.user.name {
        inherit (htgen.user) home name;
        group = htgen.user.name;
        createHome = true;
        isSystemUser = true;
      }
    ) cfg;

    users.groups = lib.mapAttrs' (
      name: htgen:
      lib.nameValuePair htgen.user.name {
        name = htgen.user.name;
      }
    ) cfg;
  };
in
out
