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
      cardo
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
      ibm-plex
      jetbrains-mono
      libertinus
      libre-bodoni
      lmodern
      merriweather
      ocr-a
      roboto
      roboto-mono
      noto-fonts
      noto-fonts-cjk
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
    fontconfig.defaultFonts = rec {
      monospace = ["Noto Sans Mono"] ++ emoji;
      serif = ["Noto Serif" "Noto Naskh Arabic" "Noto Serif Devanagari"];
      sansSerif = ["Noto Sans Display" "Noto Kufi Arabic" "Noto Sans Devanagari" "Noto Sans CJK JP"];
      emoji = ["Noto Color Emoji"];
    };
  };
}
