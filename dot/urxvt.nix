{ pkgs }:
with import ../theme.nix;
{
  enable = true;
  package = pkgs.rxvt_unicode-with-plugins;
  keybindings = {
    "Shift-Control-C" = "eval:selection_to_clipboard";
    "Shift-Control-V" = "eval:paste_clipboard";
  };
  scroll.bar.enable = false;
  extraConfig = {
    perl-ext-common = "default,clipboard,url-select,atcher";
    urlLauncher = "opera";
    "matcher.button" = 3;
    fading = 50;
    iso14755 = false;
    urgentOnBell = true;
    reverseVideo = true;
  };
}
