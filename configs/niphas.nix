{
  config,
  pkgs,
  lib,
  ...
}:
{
  niphas = {
    # wlsunset only takes fixed coordinates, so no GeoClue here
    redshift = { inherit (config.location) latitude longitude; };

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
        key = pkgs.lib.niveum.kieran.signingKey;
        behavior = "own";
        backends.ssh.allowed-signers = "~/.ssh/allowed_signers";
      };
    };

    editor.copilot = true;

    # drives niphas' Mod+Shift+W lock bind
    locker.package = pkgs.swaylock;

    # niri turns monitors back on by itself on input, hence no resume command
    idle.package =
      let
        lock = "${lib.getExe config.niphas.locker.package} -f";
      in
      pkgs.writers.writeDashBin "idle" ''
        exec ${lib.getExe pkgs.swayidle} -w \
          timeout 900 '${lock}' \
          timeout 1200 '${lib.getExe pkgs.niri} msg action power-off-monitors' \
          before-sleep '${lock}' \
          lock '${lock}'
      '';

    niri.settings = {
      layout.focus-ring.width = 1;
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
