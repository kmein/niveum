{ pkgs, config, ... }:
{
  services.urxvtd.enable = true;

  niveum.applications.terminal = "urxvtc";

  home-manager.users.me.programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    keybindings = {
      "Shift-Control-C" = "eval:selection_to_clipboard";
      "Shift-Control-V" = "eval:paste_clipboard";
    };
    scroll.bar.enable = false;
    extraConfig = {
      perl-ext = "default,url-select";
      "url-select.launcher" = "/usr/bin/env chromium";
      "url-select.underline" = true;
      "colorUL" = config.niveum.colours.blue.bright;
      "perl-lib" = "${pkgs.urxvt_perls}/lib/urxvt/perl";
      urlLauncher = "/usr/bin/env chromium";
      fading = 20;
      iso14755 = false;
      urgentOnBell = true;
      reverseVideo = false;
    };
  };
}
