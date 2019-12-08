{ config, lib, pkgs, ... }:
with lib;
let
  colourScheme = config.niveum.colours;

  my-types.hexColour = types.strMatching "#[0-9A-Fa-f]{6}";
  my-types.colourPair = types.submodule {
    options = {
      dark = mkOption { type = my-types.hexColour; };
      bright = mkOption { type = my-types.hexColour; };
    };
  };
  my-types.theme = types.submodule {
    options = {
      name = mkOption { type = types.str; };
      package = mkOption { type = types.package; };
    };
  };

  my-types.colourTheme = types.submodule {
    options = {
      black = mkOption { type = my-types.colourPair; };
      red = mkOption { type = my-types.colourPair; };
      green = mkOption { type = my-types.colourPair; };
      yellow = mkOption { type = my-types.colourPair; };
      blue = mkOption { type = my-types.colourPair; };
      magenta = mkOption { type = my-types.colourPair; };
      cyan = mkOption { type = my-types.colourPair; };
      white = mkOption { type = my-types.colourPair; };
      background = mkOption { type = my-types.hexColour; };
      foreground = mkOption { type = my-types.hexColour; };
      cursor = mkOption { type = my-types.hexColour; };
    };
  };
in {
  options.niveum = {
    applications = {
      fileManager = mkOption { type = types.str; };
    };

    colours = mkOption { type = my-types.colourTheme; };

    colourPalette = mkOption {
      type = types.listOf my-types.hexColour;
      default = with config.niveum.colours; [
        black.dark
        red.dark
        green.dark
        yellow.dark
        blue.dark
        magenta.dark
        cyan.dark
        white.dark
        black.bright
        red.bright
        green.bright
        yellow.bright
        blue.bright
        magenta.bright
        cyan.bright
        white.bright
      ];
    };

    networkInterfaces.wireless = mkOption { type = types.str; };

    batteryBlocks.default = mkOption { type = types.str; };

    promptColours =
    let colours16 = types.enum [ "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white" ];
    in {
      success = mkOption { type = colours16; default = "green"; };
      failure = mkOption { type = colours16; default = "red"; };
    };

    fonts = {
      size = mkOption { type = types.int; };
    };

    user = {
      github = mkOption { type = types.str; };
      name = mkOption { type = types.str; };
      email = mkOption { type = types.strMatching ".+@.+\\..+"; };
    };

    ignore = mkOption {
      type = types.listOf types.str;
      default = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.py[co]" "*.o" "*.hi" "*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.out" "*.run.xml" "*.toc" "*.bbl" "*.class" "*.dyn_hi" "*.dyn_o" "dist/" ];
    };

    theme = {
      gtk = mkOption { type = my-types.theme; };
      icon = mkOption { type = my-types.theme; };
      cursor = mkOption { type = my-types.theme; };
    };
  };
}
