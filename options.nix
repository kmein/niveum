{ config, lib, pkgs, ... }:
with lib;
with import ./theme.nix;
let
  unstable = import <nixos-unstable> {};
  stringOption = def: mkOption { type = types.string; default = def; };
  themeOption = def: mkOption {
    type = types.submodule {
      options = {
        name = mkOption { type = types.string; default = def.name; };
        package = mkOption { type = types.package; default = def.package; };
      };
    };
    default = def;
  };
in {
  options.defaultApplications = mapAttrs (const stringOption) rec {
    terminal = "${pkgs.rxvt_unicode-with-plugins}/bin/urxvtc";
    browser = "${pkgs.chromium}/bin/chromium";
    fileManager = "${terminal} -e ${pkgs.ranger}/bin/ranger";
    # locker = "${pkgs.i3lock}/bin/i3lock -u -c ${strings.removePrefix "#" colorScheme.background}";
    locker = "${pkgs.lightlocker}/bin/light-locker-command -l";
  };

  options.constants = {
    user = mapAttrs (const stringOption) {
      github = "kmein";
      name = "Kierán Meinhardt";
      email = "kieran.meinhardt@gmail.com";
    };

    ignore = mkOption {
      type = types.listOf types.string;
      default = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.py[co]" "*.o" "*.hi" "*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.out" "*.run.xml" "*.toc" "*.bbl" "*.class" "*.dyn_hi" "*.dyn_o" "dist/" ];
    };

    theme = mapAttrs (const themeOption) {
      gtk = { name = "Arc"; package = pkgs.arc-theme; };
      icon = { name = "Arc"; package = pkgs.arc-icon-theme; };
      cursor = { name = "capitaine-cursors"; package = pkgs.capitaine-cursors; };
    };
  };

}
