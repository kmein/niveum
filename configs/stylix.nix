{
  pkgs,
  config,
  lib,
  ...
}:
{
  # https://danth.github.io/stylix/tricks.html
  stylix.enable = true;

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/papercolor-light.yaml";

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
