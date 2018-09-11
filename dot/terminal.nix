{ defaultApplications }:
with import ../theme.nix;
''
[Configuration]
FontName=${terminalFont.regular.name} ${toString terminalFont.size}
MiscAlwaysShowTabs=FALSE
MiscBell=TRUE
MiscMenubarDefault=FALSE
MiscToolbarDefault=FALSE
MiscConfirmClose=FALSE
MiscCursorBlinks=TRUE
MiscCursorShape=TERMINAL_CURSOR_SHAPE_BLOCK
MiscBordersDefault=FALSE
MiscMiddleClickOpensUri=TRUE
MiscMouseWheelZoom=TRUE
MiscRewrapOnResize=TRUE
TitleMode=TERMINAL_TITLE_HIDE
MiscMouseAutohide=FALSE
MiscHighlightUrls=TRUE
BackgroundDarkness=0.95
BackgroundMode=TERMINAL_BACKGROUND_TRANSPARENT
ColorPalette=${dark};${red.dark};${green.dark};${yellow.dark};${blue.dark};${magenta.dark};${cyan.dark};${light};${dark};${red.light};${green.light};${yellow.light};${blue.light};${magenta.light};${cyan.light};${light}
ColorBackground=${veryDark}
ColorForeground=${light}
ColorCursor=${light}
''
