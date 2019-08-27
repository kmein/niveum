{ lib }:
{
  commaSep = builtins.concatStringsSep ",";
  strip = lib.strings.removeSuffix "\n";
}
