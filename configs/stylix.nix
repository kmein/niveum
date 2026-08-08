{
  pkgs,
  config,
  lib,
  ...
}:
{
  # https://danth.github.io/stylix/tricks.html
  stylix.enable = true;

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-light-medium.yaml";

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

  # vim follows the stylix scheme; single source of truth for the palette
  niphas.editor.stylixColors = config.lib.stylix.colors;

  # waybar follows the scheme too. Its stylesheet is plain CSS, so the
  # background goes through rgba() to pick up stylix' desktop opacity;
  # the rest are hex.
  niphas.bar.colors =
    let
      inherit (config.lib.stylix) colors;
    in
    {
      background = "rgba(${colors.base00-rgb-r}, ${colors.base00-rgb-g}, ${colors.base00-rgb-b}, ${toString config.stylix.opacity.desktop})";
      foreground = colors.withHashtag.base05;
      muted = colors.withHashtag.base04;
      accent = colors.withHashtag.base0D;
      warning = colors.withHashtag.base0A;
      critical = colors.withHashtag.base08;
    };

  # wallpaper generated from the scheme; machine configs may override
  # (e.g. fatteh's applicative wallpaper)
  niphas.wallpaper.image = lib.mkDefault (
    pkgs.callPackage ../packages/niveum-wallpaper.nix {
      inherit (config.lib.stylix) colors;
    }
  );

  stylix.fonts = {
    serif = {
      package = pkgs.noto-fonts;
      name = "Noto Serif";
    };

    sansSerif = {
      package = pkgs.inter;
      name = "Inter";
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
