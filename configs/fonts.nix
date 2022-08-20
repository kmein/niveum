{pkgs, ...}: {
  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    fonts = with pkgs; [
      alegreya
      alegreya-sans
      amiri
      annapurna-sil
      cantarell-fonts
      charis-sil
      corefonts
      crimson
      eb-garamond
      font-awesome_6
      etBook
      ezra-sil
      fira
      font-awesome
      galatia-sil
      gentium
      gfs-fonts
      gyre-fonts
      ia-writer-duospace
      ibm-plex
      jetbrains-mono
      libertinus
      libre-bodoni
      lmodern
      merriweather
      ocr-a
      roboto
      roboto-mono
      noto-fonts-emoji
      roboto-slab
      scheherazade-new
      source-code-pro
      source-sans-pro
      source-serif-pro
      theano
      tocharian-font
      vistafonts
      vollkorn
      zilla-slab
    ]; # google-fonts league-of-moveable-type
    fontconfig.defaultFonts = let
      emojiFont = "Noto Emoji";
    in {
      monospace = ["JetBrains Mono" emojiFont];
      serif = ["Merriweather"];
      sansSerif = ["Cantarell" emojiFont];
      emoji = [emojiFont];
    };
  };
}
