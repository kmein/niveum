{
  config,
  lib,
  ...
}:
let
  format = config.pkgs.formats.toml { };
in
{
  _class = "wrapper";
  options.settings = lib.mkOption {
    inherit (format) type;
    default = { };
    description = "iamb's config.toml, see iamb(5).";
  };
  config = {
    package = lib.mkDefault config.pkgs.iamb;
    # -C names the directory holding iamb/config.toml; session and keys go to
    # the data directory, so the store copy can stay read-only
    flags."-C" = config.pkgs.linkFarm "iamb-config" {
      "iamb/config.toml" = format.generate "config.toml" config.settings;
    };
  };
}
