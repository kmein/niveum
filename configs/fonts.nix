{ pkgs, ... }:
{
  niveum.fonts.size = 9;

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
      roboto
      roboto-mono
      roboto-slab
      source-code-pro
      source-serif-pro
      ubuntu_font_family
      gfs-fonts
      unstable.jetbrains-mono
      twemoji-color-font
      joypixels
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
        serif = [ "IBM Plex Serif" ];
        sansSerif = [ "IBM Plex Sans" ];
        emoji = [ "JoyPixels" ];
      };
      ultimate = {
        enable = true;
        substitutions = "combi";
      };
    };
  };
}
