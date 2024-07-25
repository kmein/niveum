{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  generatedWallpaper = pkgs.runCommand "wallpaper.png" {} ''
    ${inputs.wallpaper-generator.packages.x86_64-linux.wp-gen}/bin/wallpaper-generator lines \
      --output $out \
      ${lib.concatMapStringsSep " "
      (n: "--base0${lib.toHexString n}=${config.lib.stylix.colors.withHashtag."base0${lib.toHexString n}"}")
      (lib.range 0 15)}
  '';
in {
  # https://danth.github.io/stylix/tricks.html
  # stylix.image = inputs.wallpapers.outPath + "/meteora/rodrigo-soares-250630.jpg";
  stylix.enable = true;
  stylix.image = generatedWallpaper;

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
      package = pkgs.noto-fonts;
      name = "Noto Serif";
    };

    sansSerif = {
      package = pkgs.noto-fonts;
      name = "Noto Sans";
    };

    monospace = {
      package = pkgs.noto-fonts;
      name = "Noto Sans Mono";
    };

    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };

    sizes = {
      terminal = 6;
      applications = 10;
    };
  };
}
