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
      niphas-wallpaper = prev.callPackage ../packages/applicative-wallpaper.nix {
        inherit (config.lib.stylix) colors;
      };
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
