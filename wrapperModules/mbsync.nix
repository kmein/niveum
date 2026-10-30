{
  config,
  lib,
  wlib,
  ...
}:
let
  inherit (lib) mkOption types;
  # written in this order, so stores come after the accounts they use and
  # channels after the stores
  kinds = [
    "IMAPAccount"
    "IMAPStore"
    "MaildirStore"
    "Channel"
    "Group"
  ];
  value = types.oneOf [
    types.str
    types.int
    types.bool
    (types.listOf types.str)
  ];

  quote =
    s:
    if s != "" && builtins.match ''[^[:space:]"\\]+'' s != null then
      s
    else
      ''"${lib.escape [ ''"'' ''\'' ] s}"'';
  render =
    v:
    if lib.isBool v then
      (if v then "yes" else "no")
    else if lib.isInt v then
      toString v
    else if lib.isList v then
      lib.concatMapStringsSep " " quote v
    else
      quote v;
  section =
    kind: name: settings:
    "${kind} ${name}\n" + lib.concatStrings (lib.mapAttrsToList (k: v: "${k} ${render v}\n") settings);
in
{
  _class = "wrapper";
  options = {
    settings = lib.genAttrs kinds (
      kind:
      mkOption {
        type = types.attrsOf (types.attrsOf value);
        default = { };
        description = "`${kind}` sections by name, see mbsync(1).";
      }
    );
    mbsyncrc = mkOption { type = wlib.types.file config.pkgs; };
  };
  config = {
    package = lib.mkDefault config.pkgs.isync;
    exePath = "${config.package}/bin/mbsync";
    # mkDefault, so that setting `mbsyncrc.path` or `.content` replaces it
    mbsyncrc.content = lib.mkDefault (
      lib.concatStringsSep "\n" (
        lib.concatMap (kind: lib.mapAttrsToList (section kind) config.settings.${kind}) kinds
      )
    );
    flags."--config" = config.mbsyncrc.path;
  };
}
