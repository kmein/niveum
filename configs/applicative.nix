{
  config,
  pkgs,
  lib,
  ...
}:
{
  users.users.applicative = {
    name = "asg";
    description = "Applicative Systems";
    hashedPasswordFile = config.age.secrets.kfm-password.path;
    home = "/home/applicative";
    uid = 1001;
    isNormalUser = true;
    extraGroups = [
      "pipewire"
      "audio"
    ];
  };

  nixpkgs.overlays = [
    (final: prev: {
      niphas-wallpaper =
        let
          backgroundColor = config.lib.stylix.colors.withHashtag.base06;
          foregroundColor = config.lib.stylix.colors.withHashtag.base01;
          width = 1920;
          height = 1080;

          svgUrl = "https://applicative.systems/_astro/logo-full.D8zRvqBZ.svg";
          logoSvg = prev.fetchurl {
            url = svgUrl;
            hash = "sha256-qXDIEZsAPn4eUJ3kb5U6L3PMUCtWGYqhqyIaBt7FntE=";
          };
        in
        prev.runCommand "applicative-wallpaper.png"
          {
            nativeBuildInputs = [ prev.imagemagick ];
          }
          ''
            # 1. We use -background to set the canvas color
            # 2. We use -fuzz and -opaque to replace the logo's internal colors
            # 3. We use -gravity and -extent to center it on a wallpaper-sized canvas

            convert \
              -background none \
              -density 300 \
              "${logoSvg}" \
              -fuzz 100% -fill "${foregroundColor}" -opaque black \
              -resize 800x800 \
              -gravity center \
              -background "${backgroundColor}" \
              -extent ${toString width}x${toString height} \
              $out
          '';
    })
  ];

  services.getty.autologinOnce = lib.mkForce false;

  # to run nspawn in nix sandbox
  nix.settings = {
    auto-allocate-uids = true;
    system-features = [ "uid-range" ];
    experimental-features = [
      "auto-allocate-uids"
      "cgroups"
    ];
    trusted-users = [ config.users.users.applicative.name ];
  };

  services.restic.backups.niveum = {
    extraBackupArgs = [
      "--exclude=${config.users.users.applicative.home}/src/nixpkgs/.git"
    ];
    paths = [
      config.users.users.applicative.home
    ];
  };

  security.sudo.extraRules = [
    {
      # still required for systemd-nspawn
      users = [ config.users.users.applicative.name ];
      commands = [ "ALL" ];
    }
  ];
}
