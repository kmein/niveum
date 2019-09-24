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
    fonts = with pkgs; [ inconsolata corefonts eb-garamond fira libertine lmodern noto-fonts roboto roboto-mono roboto-slab ubuntu_font_family ];
    fontconfig = {
      defaultFonts.monospace = [ "Inconsolata" ];
      ultimate = {
        enable = true;
        substitutions = "combi";
      };
    };
  };
}
