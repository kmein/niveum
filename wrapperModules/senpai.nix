{
  config,
  lib,
  wlib,
  ...
}:
let
  inherit (lib) mkOption types;
  word =
    v:
    if lib.isBool v then
      lib.boolToString v
    else if lib.isInt v then
      toString v
    else
      "\"${lib.escape [ "\\" "\"" ] v}\"";
  render =
    indent: settings:
    lib.concatStrings (
      lib.mapAttrsToList (
        name: value:
        if lib.isAttrs value then
          "${indent}${name} {\n${render "${indent}  " value}${indent}}\n"
        else
          "${indent}${name} ${lib.concatMapStringsSep " " word (lib.toList value)}\n"
      ) settings
    );
  value = types.oneOf [
    types.str
    types.int
    types.bool
    (types.listOf types.str)
  ];
in
{
  _class = "wrapper";
  options = {
    settings = mkOption {
      type = types.attrsOf (types.either value (types.attrsOf value));
      default = { };
      description = ''
        senpai.scfg directives, see senpai(5): a list becomes several
        parameters, an attribute set a block. The file is world-readable in
        the store: reference the password via `password-cmd`.
      '';
    };
    "senpai.scfg" = mkOption { type = wlib.types.file config.pkgs; };
  };
  config = {
    package = lib.mkDefault config.pkgs.senpai;
    "senpai.scfg".content = lib.mkDefault (render "" config.settings);
    flags."-config" = config."senpai.scfg".path;
  };
}
