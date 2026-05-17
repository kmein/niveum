{
  pkgs,
  config,
  lib,
  ...
}:
let
# Define the high-contrast monochrome palette directly in Nix
  linocut-dark = {
    base00 = "000000"; # Default Background (Pure Black)
    base01 = "1c1c1c"; # Lighter Background
    base02 = "3a3a3a"; # Selection Background
    base03 = "767676"; # Comments, Invisibles
    base04 = "9e9e9e"; # Dark Foreground
    base05 = "e4e4e4"; # Default Foreground
    base06 = "f0f0f0"; # Light Foreground
    base07 = "ffffff"; # Brightest Foreground
    base08 = "d0d0d0"; # Variables
    base09 = "bcbcbc"; # Integers, Constants
    base0A = "ffffff"; # Classes
    base0B = "b2b2b2"; # Strings
    base0C = "cccccc"; # Regex
    base0D = "ffffff"; # Functions
    base0E = "eaeaea"; # Keywords
    base0F = "585858"; # Deprecated
  };

  linocut-light = {
    base00 = "ffffff"; # Default Background (Pure White)
    base01 = "f0f0f0"; # Lighter Background
    base02 = "e4e4e4"; # Selection Background
    base03 = "6c6c6c"; # Comments, Invisibles
    base04 = "585858"; # Dark Foreground
    base05 = "1c1c1c"; # Default Foreground
    base06 = "0d0d0d"; # Dark Foreground
    base07 = "000000"; # Brightest Foreground
    base08 = "3a3a3a"; # Variables
    base09 = "585858"; # Integers, Constants
    base0A = "000000"; # Classes
    base0B = "4e4e4e"; # Strings
    base0C = "3a3a3a"; # Regex
    base0D = "000000"; # Functions
    base0E = "1c1c1c"; # Keywords
    base0F = "bcbcbc"; # Deprecated
  };
in
{
  stylix.base16Scheme = linocut-light;
  stylix.polarity = "light";

  # https://danth.github.io/stylix/tricks.html
  stylix.enable = true;


  stylix.cursor = {
    name = "capitaine-cursors-white";
    package = pkgs.capitaine-cursors;
    size = 12;
  };

  home-manager.users.me = {
    stylix.autoEnable = true;
  };

  # environment.etc."stylix/wallpaper.png".source = generatedWallpaper;

  # stylix.polarity = "either";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/${
  # onedark
  # synth-midnight-dark
  # apprentice # https://romainl.github.io/Apprentice/
  # one-light
  # onedark
  # material # https://github.com/ntpeters/base16-materialtheme-scheme
  # material-palenight
  # material-lighter
  # tomorrow # https://github.com/chriskempson/tomorrow-theme
  # tomorrow-night
  # gruvbox-light-medium # https://github.com/dawikur/base16-gruvbox-scheme
  # gruvbox-dark-medium
  # selenized-light # https://github.com/jan-warchol/selenized
  # selenized-dark
  # papercolor-light
  # papercolor-dark
  # dracula # https://draculatheme.com/
  # }.yaml";

  stylix.fonts = {
    serif = {
      package = pkgs.iosevka-bin.override { variant = "Etoile"; };
      name = "Iosevka Etoile";
    };

    sansSerif = {
      package = pkgs.iosevka-bin.override { variant = "Etoile"; };
      name = "Iosevka Etoile";
    };

    monospace = {
      package = pkgs.iosevka-bin;
      name = "Iosevka Extended";
    };

    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
    };

    sizes = {
      terminal = 8;
      applications = 10;
    };
  };
}
