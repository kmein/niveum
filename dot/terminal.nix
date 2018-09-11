{ defaultApplications }:
with import ../theme.nix;
''
[Configuration]
BackgroundDarkness=0.95
BackgroundMode=TERMINAL_BACKGROUND_TRANSPARENT
ColorBackground=${veryDark}
ColorCursor=${light}
ColorForeground=${light}
ColorPalette=${dark};${red.dark};${green.dark};${yellow.dark};${blue.dark};${magenta.dark};${cyan.dark};${light};${dark};${red.light};${green.light};${yellow.light};${blue.light};${magenta.light};${cyan.light};${light}
FontName=${terminalFont.regular.name} ${toString terminalFont.size}
MiscAlwaysShowTabs=FALSE
MiscBell=TRUE
MiscBordersDefault=FALSE
MiscConfirmClose=FALSE
MiscCursorBlinks=TRUE
MiscCursorShape=TERMINAL_CURSOR_SHAPE_BLOCK
MiscHighlightUrls=TRUE
MiscMenubarDefault=FALSE
MiscMiddleClickOpensUri=TRUE
MiscMouseAutohide=FALSE
MiscMouseWheelZoom=TRUE
MiscRewrapOnResize=TRUE
MiscToolbarDefault=FALSE
ScrollingBar=TERMINAL_SCROLLBAR_NONE
TitleMode=TERMINAL_TITLE_HIDE
''
