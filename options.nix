{ lib, pkgs, ... }:
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
  options.defaultApplications = mapAttrs (const stringOption) {
    terminal = "${pkgs.xfce.terminal}/bin/xfce4-terminal";
    browser = "${pkgs.chromium}/bin/chromium";
    fileManager = "${pkgs.xfce.thunar}/bin/thunar";
    locker = "${pkgs.i3lock}/bin/i3lock -e -c ${strings.removePrefix "#" black}";
  };

  options.constants = {
    user = mapAttrs (const stringOption) {
      github = "kmein";
      name = "Kierán Meinhardt";
      email = "kieran.meinhardt@gmail.com";
    };

    ignore = mkOption {
      type = types.listOf types.string;
      default = [ "*~" ".stack-work/" "__pycache__/" ".mypy_cache/" "*.o" "*.hi" "*.aux" "*.class" "*.dyn_hi" "*.dyn_o" "dist/" ];
    };

    theme = mapAttrs (const themeOption) {
      gtk = { name = "Paper"; package = pkgs.paper-gtk-theme; };
      icon = { name = "Paper"; package = pkgs.paper-icon-theme; };
    };

    wallpaper = mkOption {
      type = types.path;
      default = pkgs.copyPathToStore ./art/haskell-grey.png;
    };
  };

}
