{ config, lib, pkgs, ... }:
with lib;
with import <dot/theme.nix>;
let
  stringOption = mkOption { type = types.string; };
  themeOption = mkOption {
    type = types.submodule {
      options = {
        name = mkOption { type = types.string; };
        package = mkOption { type = types.package; };
      };
    };
  };
in {
  options.niveum = {
    applications = {
      terminal = stringOption;
      browser = stringOption;
      fileManager = stringOption;
    };

    user = {
      github = stringOption;
      name = stringOption;
      email = stringOption;
    };

    ignore = mkOption {
      type = types.listOf types.string;
      default = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.py[co]" "*.o" "*.hi" "*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.out" "*.run.xml" "*.toc" "*.bbl" "*.class" "*.dyn_hi" "*.dyn_o" "dist/" ];
    };

    theme = {
      gtk = themeOption;
      icon = themeOption;
      cursor = themeOption;
    };
  };
}
