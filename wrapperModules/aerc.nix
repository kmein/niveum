{
  config,
  lib,
  wlib,
  ...
}:
let
  inherit (lib) mkOption types;
  value = types.oneOf [
    types.str
    types.int
    types.bool
    (types.listOf types.str)
  ];
  sections = types.attrsOf (types.attrsOf value);

  keyValue = lib.generators.toKeyValue {
    mkKeyValue = lib.generators.mkKeyValueDefault {
      mkValueString =
        v: if lib.isList v then lib.concatStringsSep "," v else lib.generators.mkValueStringDefault { } v;
    } " = ";
  };
  section = name: values: "[${name}]\n" + keyValue values;
  toINI = lib.mapAttrsToList section;
in
{
  _class = "wrapper";
  options = {
    settings = mkOption {
      type = sections;
      default = { };
      description = "aerc.conf sections, see aerc-config(5).";
    };
    binds = mkOption {
      type = types.attrsOf (types.attrsOf types.str);
      default = { };
      description = "binds.conf contexts, see aerc-binds(5); `global` is written before the first section header.";
    };
    accounts = mkOption {
      type = sections;
      default = { };
      description = ''
        accounts.conf sections, see aerc-accounts(5). The file is world-readable
        in the store: reference passwords via `source-cred-cmd`/`outgoing-cred-cmd`.
      '';
    };
    accountOrder = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Accounts whose tabs come first, in this order (aerc opens the first); the rest follow alphabetically.";
    };
    "aerc.conf" = mkOption { type = wlib.types.file config.pkgs; };
    "binds.conf" = mkOption { type = wlib.types.file config.pkgs; };
    "accounts.conf" = mkOption { type = wlib.types.file config.pkgs; };
  };
  config = {
    package = lib.mkDefault config.pkgs.aerc;
    # accounts.conf is a store path and therefore not 0600
    settings.general.unsafe-accounts-conf = true;
    # mkDefault, so that setting a file's `path` or `content` replaces it
    "aerc.conf".content = lib.mkDefault (lib.concatStringsSep "\n" (toINI config.settings));
    "binds.conf".content = lib.mkDefault (
      lib.concatStringsSep "\n" (
        lib.optional (config.binds ? global) (keyValue config.binds.global)
        ++ toINI (removeAttrs config.binds [ "global" ])
      )
    );
    "accounts.conf".content =
      let
        order =
          config.accountOrder ++ lib.subtractLists config.accountOrder (lib.attrNames config.accounts);
      in
      lib.mkDefault (lib.concatMapStringsSep "\n" (name: section name config.accounts.${name}) order);
    flags = {
      "--aerc-conf" = config."aerc.conf".path;
      "--binds-conf" = config."binds.conf".path;
      "--accounts-conf" = config."accounts.conf".path;
    };
  };
}
