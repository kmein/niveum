{ config, pkgs }:
with import <dot/theme.nix>;
{
  enable = true;
  package = pkgs.rxvt_unicode-with-plugins;
  keybindings = {
    "Shift-Control-C" = "eval:selection_to_clipboard";
    "Shift-Control-V" = "eval:paste_clipboard";
  };
  scroll.bar.enable = false;
  extraConfig = {
    perl-ext-common = "default,clipboard,url-select,matcher";
    urlLauncher = "/usr/bin/env chromium";
    fading = 20;
    iso14755 = false;
    urgentOnBell = true;
    reverseVideo = false;
  };
}
