{
  pkgs,
  inputs,
  ...
}: {
  # https://danth.github.io/stylix/tricks.html
  stylix.image = inputs.wallpapers.outPath + "/vaporwave/1432599578099.png";
  # stylix.polarity = "either";
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/synth-midnight-dark.yaml";

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
