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
    opencrow-gemini-key = {
      file = ../../secrets/opencrow-gemini-key.age;
    };
    opencrow-openrouter-key = {
      file = ../../secrets/opencrow-openrouter-key.age;
    };
  };

  environment.systemPackages = [
    pkgs.pi-coding-agent
  ];

  services.opencrow = {
    enable = true;

    package = pkgs.opencrow;
    piPackage = pkgs.pi-coding-agent;

    extraPackages = [
      pkgs.pi-coding-agent
      pkgs.nix
    ];

    environmentFiles = [
      config.age.secrets.opencrow-matrix-token.path
      config.age.secrets.opencrow-openrouter-key.path
      config.age.secrets.opencrow-gemini-key.path
    ];

    extraBindMounts."/run/opencrow/SOUL.md" = {
      hostPath = config.age.secrets.opencrow-soul.path;
      isReadOnly = true;
    };

    environment = {
      NIX_REMOTE = "daemon";

      PI_PERMISSION_LEVEL = "high";
      OPENCROW_MATRIX_HOMESERVER = "https://matrix.org";
      OPENCROW_MATRIX_USER_ID = "@fable_ai:matrix.org";
      OPENCROW_SOUL_FILE = "/run/opencrow/SOUL.md";
      OPENCROW_HEARTBEAT_INTERVAL = "2h";

      # end of the month
      OPENCROW_PI_PROVIDER = "openrouter";
      OPENCROW_PI_MODEL = "stepfun/step-3.5-flash:free";
      # OPENCROW_PI_PROVIDER = "google";
      # OPENCROW_PI_MODEL = "gemini-2.0-flash";

      # beginning of the month
      # OPENCROW_PI_PROVIDER = "github-copilot";
      # OPENCROW_PI_MODEL = "claude-opus-4.6";
    };
  };

  containers.opencrow.config = {
    nix.settings.experimental-features = [
      "flakes"
      "nix-command"
    ];
  };

  nix.settings.experimental-features = [
    "flakes"
    "nix-command"
  ];

  services.restic.backups.niveum.paths = [
    "/var/lib/opencrow"
  ];
}
