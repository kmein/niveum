{ config, lib, pkgs, ... }:
with lib;
{
  options.niveum = {
    wirelessInterface = mkOption { type = types.str; };

    batteryName = mkOption { type = types.str; };

    promptColours = let
      colours16 = types.enum [
        "black"
        "red"
        "green"
        "yellow"
        "blue"
        "magenta"
        "cyan"
        "white"
      ];
    in {
      success = mkOption {
        type = colours16;
        default = "green";
      };
      failure = mkOption {
        type = colours16;
        default = "red";
      };
    };
  };
}
