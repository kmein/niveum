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
    opacityRules = [
      # opacity-rule overrides both inactive and active opacity
      "100:class_g = 'slock'" # don't want a transparent lock screen!

      # video in browser tabs
      # substring /regex match of title bar text
      "100:name *?= 'Youtube'"
      "100:name *?= 'slock'"
    ];
  };
}
