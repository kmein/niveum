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

    environmentFiles = [
      config.age.secrets.opencrow-matrix-token.path
    ];

    extraBindMounts."/run/secrets/opencrow-soul" = {
      hostPath = config.age.secrets.opencrow-soul.path;
      isReadOnly = true;
    };

    environment = {
      OPENCROW_MATRIX_HOMESERVER = "https://matrix.4d2.org";
      OPENCROW_MATRIX_USER_ID = "@fable:4d2.org";
      OPENCROW_PI_PROVIDER = "copilot";
      OPENCROW_PI_MODEL = "claude-sonnet-4-20250514";
      OPENCROW_SOUL_FILE = "/run/secrets/opencrow-soul";
    };
  };

  services.restic.backups.niveum.paths = [
    "/var/lib/opencrow"
  ];
}
