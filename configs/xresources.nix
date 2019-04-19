{ config, lib, ... }:
{
  home-manager.users.me.xresources.properties = with config.niveum; {
    "*background" = colours.background;
    "*foreground" = colours.foreground;
    "*fadeColor" = colours.background;
    "*cursorColor" = colours.cursor;
    "*.font" = "xft:${fonts.terminal.name}:size=${toString fonts.terminal.size}";
    "*.boldFont" = "xft:${fonts.terminal.name}:style=Bold:size=${toString fonts.terminal.size}";
    "*.italicFont" = "xft:${fonts.terminal.name}:style=Italic:size=${toString fonts.terminal.size}";
  } // lib.lists.foldr (i: cs: cs // { "*color${toString i}" = builtins.elemAt colourPalette i; }) {} (lib.lists.range 0 15);
}
