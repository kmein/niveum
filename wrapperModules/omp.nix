# omp (oh my pi) with settings as a config overlay: it outranks
# ~/.omp/agent/config.yml, which omp rewrites at runtime, and is never written back.
{
  config,
  lib,
  wlib,
  ...
}:
let
  yamlFmt = config.pkgs.formats.yaml { };
in
{
  _class = "wrapper";
  options = {
    settings = lib.mkOption {
      type = yamlFmt.type;
      default = { };
      description = "omp settings; lists replace those in config.yml instead of extending them.";
    };
    configFile = lib.mkOption {
      type = wlib.types.file config.pkgs;
      default.path = toString (yamlFmt.generate "omp.yml" config.settings);
    };
  };
  config = {
    package = lib.mkDefault config.pkgs.omp;
    env.PI_CONFIG_FILES = config.configFile.path;
  };
}
