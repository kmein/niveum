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
      # bind-mounted into the container, where opencrow runs as an unprivileged
      # user that can't read the root-owned 0400 default
      mode = "0444";
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
    # opencrow switched from pi to its successor omp (Oh My Pi); it looks up
    # an `omp` binary, which llm-agents.nix packages
    piPackage = pkgs.omp;

    extraPackages = [
      pkgs.omp
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
      OPENCROW_PI_MODEL = "moonshotai/kimi-k2.6:free";

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

  # binary cache of llm-agents.nix, which packages omp
  nix.settings.extra-substituters = [ "https://cache.numtide.com" ];
  nix.settings.extra-trusted-public-keys = [
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
  ];

  services.restic.backups.niveum.paths = [
    "/var/lib/opencrow"
  ];
}
