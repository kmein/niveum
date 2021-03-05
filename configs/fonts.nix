{ pkgs, ... }: {
  fonts = {
    enableDefaultFonts = true;
    enableFontDir = true;
    fonts = with pkgs; [
      corefonts
      eb-garamond
      fira
      font-awesome-ttf
      ibm-plex
      inconsolata
      iosevka
      libertine
      lmodern
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto
      roboto-mono
      roboto-slab
      source-code-pro
      source-serif-pro
      source-sans-pro
      ubuntu_font_family
      gfs-fonts
      jetbrains-mono
      twemoji-color-font
      joypixels
    ];
    fontconfig.defaultFonts = {
      monospace = [ "JetBrains Mono" ];
      serif = [ "Roboto Slab" ];
      sansSerif = [ "Roboto" "Noto Sans" ];
      emoji = [ "JoyPixels" ];
    };
  };
}
