{ pkgs, ... }:
{
  niveum.fonts = {
    ui = {
      name = "Sans";
      size = 9;
    };
    terminal = {
      name = "Monospace";
      size = 9;
    };
  };

  fonts = {
    enableDefaultFonts = true;
    enableFontDir = true;
    fonts = with pkgs; [ corefonts eb-garamond fira libertine lmodern noto-fonts roboto ubuntu_font_family ];
  };
}
