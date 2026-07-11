{
  pkgs,
  lib,
  ...
}:
{
  niphas = {
    # unwrapped ashell reads ~/.config/ashell written by home-manager
    # (programs.ashell + stylix); the wrapped bar would bypass it
    bar.package = pkgs.ashell;

    git.settings.user = {
      inherit (pkgs.lib.niveum.kieran) name email;
    };

    editor.copilot = true;

    # hyprlock over swaylock: it authenticates fingerprint (fprintd D-Bus) and
    # password in parallel, so typing your password unlocks instantly instead of
    # blocking on a swipe first. Drives niphas' Mod+Shift+W lock bind.
    locker.package = pkgs.hyprlock;

    niri.settings = {
      layout.focus-ring = {
        width = 1;
        active-color = "#000";
      };
      binds = {
        "Mod+Return".spawn-sh = "alacritty";
        "Mod+U".spawn-sh = lib.getExe pkgs.unicodmenu;
        "Mod+P".spawn-sh = lib.getExe pkgs.rofi-pass-wayland;
        "Mod+F12".spawn-sh = lib.getExe (
          pkgs.klem.override {
            options = import ../packages/klem/kmein.nix { inherit pkgs; };
          }
        );
      };
    };
  };
}
