{
  config,
  pkgs,
  ...
}:
{
  age.secrets = {
    opencrow-matrix-token = {
      file = ../../secrets/opencrow-matrix-token.age;
    };
    opencrow-soul = {
      file = ../../secrets/opencrow-soul.age;
    };
  };

  services.opencrow = {
    enable = true;

    extraPackages = [ pkgs.pi pkgs.nix ];

    environmentFiles = [
      config.age.secrets.opencrow-matrix-token.path
    ];

    extraBindMounts."/run/secrets/opencrow-soul" = {
      hostPath = config.age.secrets.opencrow-soul.path;
      isReadOnly = true;
    };

    extraBindMounts."/nix/var/nix/daemon-socket" = {
      hostPath = "/nix/var/nix/daemon-socket";
      isReadOnly = false;
     };

    environment = {
      PI_PERMISSION_LEVEL= "high";
      OPENCROW_MATRIX_HOMESERVER = "https://matrix.4d2.org";
      OPENCROW_MATRIX_USER_ID = "@fable:4d2.org";
      OPENCROW_PI_PROVIDER = "github-copilot";
      OPENCROW_PI_MODEL = "gemini-3-flash-preview";
      OPENCROW_SOUL_FILE = "/run/secrets/opencrow-soul";
    };
  };

  services.restic.backups.niveum.paths = [
    "/var/lib/opencrow"
  ];
}
