{ config, lib, ... }:
let
  inherit (lib.attrsets) nameValuePair listToAttrs;
  inherit (lib.lists) imap0;
in
{
  home-manager.users.me.xresources.properties = with config.niveum; {
    "*background" = colours.background;
    "*foreground" = colours.foreground;
    "*fadeColor" = colours.background;
    "*cursorColor" = colours.cursor;
    "*.font" = "xft:${fonts.terminal.name}:size=${toString fonts.terminal.size}";
    "*.boldFont" = "xft:${fonts.terminal.name}:style=Bold:size=${toString fonts.terminal.size}";
    "*.italicFont" = "xft:${fonts.terminal.name}:style=Italic:size=${toString fonts.terminal.size}";
  } // listToAttrs (imap0 (i: c: nameValuePair "*color${toString i}" c) colourPalette);
}
