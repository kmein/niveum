{pkgs, ...}: {
  nixpkgs.config.joypixels.acceptLicense = true;
  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    fonts = with pkgs; [
      alegreya
      alegreya-sans
      amiri
      unstable.annapurna-sil
      cantarell-fonts
      charis-sil
      corefonts
      crimson
      eb-garamond
      etBook
      unstable.ezra-sil
      fira
      font-awesome-ttf
      unstable.galatia-sil
      gentium
      gfs-fonts
      gyre-fonts
      ia-writer-duospace
      ibm-plex
      jetbrains-mono
      joypixels
      libertinus
      libre-bodoni
      lmodern
      merriweather
      ocr-a
      roboto
      roboto-mono
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
      emojiFont = "JoyPixels";
    in {
      monospace = ["JetBrains Mono" emojiFont];
      serif = ["Merriweather"];
      sansSerif = ["Cantarell" emojiFont];
      emoji = [emojiFont];
    };
  };
}
