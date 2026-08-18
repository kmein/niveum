{
  pkgs,
  lib,
  ...
}:
{
  niphas = {
    git.settings = {
      gpg = {
        format = "ssh";
        ssh.allowedSignersFile = "~/.ssh/allowed_signers";
      };
      commit.gpgsign = true;
      user = {
        signingKey = "~/.ssh/id_ed25519.pub";
        inherit (pkgs.lib.niveum.kieran) name email;
      };
    };
    jj.settings = {
      user = {
        inherit (pkgs.lib.niveum.kieran) name email;
      };
      signing = {
        backend = "ssh";
        key = pkgs.lib.niveum.machines.kabsa.sshKey;
        behavior = "own";
        backends.ssh.allowed-signers = "~/.ssh/allowed_signers";
      };
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
