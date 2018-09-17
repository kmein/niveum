with import ../theme.nix;
''
[Configuration]
BackgroundDarkness=0.9
BackgroundMode=TERMINAL_BACKGROUND_TRANSPARENT
ColorBackground=${veryDark}
ColorCursor=${gray.light}
ColorForeground=${gray.light}
ColorPalette=${builtins.concatStringsSep ";" colorPalette}
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
