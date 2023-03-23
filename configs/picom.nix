{
  services.picom = {
    enable = true;
    activeOpacity = 1;
    fade = true;
    fadeDelta = 1;
    inactiveOpacity = 0.9;
    shadow = true;
    menuOpacity = 0.9;
    shadowOpacity = 0.3;
    fadeExclude = [
      "class_g = 'slock'" # don't want a transparent lock screen!
      "name *?= 'slock'"
      "focused = 1"
    ];
    opacityRules = [
      # opacity-rule overrides both inactive and active opacity

      # video in browser tabs
      # substring /regex match of title bar text
      "99:name *?= 'Youtube'"
      "99:WM_CLASS@:s *= 'mpv$'"
    ];
  };
}
