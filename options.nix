{ config, lib, pkgs, ... }:
with lib;
with import ./theme.nix;
let
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
    locker = "${pkgs.i3lock}/bin/i3lock -u -c ${strings.removePrefix "#" colorScheme.background}";
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
      gtk = { name = "Breeze-Dark"; package = pkgs.breeze-gtk; };
      icon = { name = "Adwaita"; package = pkgs.gnome3.adwaita-icon-theme; };
      cursor = { name = "capitaine-cursors"; package = pkgs.capitaine-cursors; };
    };
  };

}
