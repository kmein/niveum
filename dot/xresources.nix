{ lib }:
let theme = import <dot/theme.nix>;
in with lib; lists.foldr
  (i: cs: cs // { "*color${toString i}" = builtins.elemAt theme.colorPalette i; })
  {
    "*background" = theme.colorScheme.background;
    "*foreground" = theme.colorScheme.foreground;
    "*fadeColor" = theme.colorScheme.background;
    "*cursorColor" = theme.colorScheme.cursorColor;
    "*.font" = "xft:${theme.terminalFont.name}:size=${toString theme.terminalFont.size}";
    "*.boldFont" = "xft:${theme.terminalFont.name}:style=Bold:size=${toString theme.terminalFont.size}";
    "*.italicFont" = "xft:${theme.terminalFont.name}:style=Italic:size=${toString theme.terminalFont.size}";
  }
  (lists.range 0 15)
