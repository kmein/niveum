{
  config,
  lib,
  pkgs,
  ...
}:
{
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    extraGroups = [ "openclaw-shared" ]; # Access to shared data
    home = "/var/lib/openclaw";
    createHome = true;
    shell = pkgs.bash;
    packages = [
      pkgs.llm-agents.openclaw
    ];
  };

  users.groups.openclaw = { };
  users.groups.openclaw-shared = { };

  systemd.services.openclaw = {
    description = "OpenClaw Gateway Service";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    path = config.users.users.openclaw.packages;

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      StateDirectory = "openclaw";
      WorkingDirectory = "/var/lib/openclaw";

      ExecStart = pkgs.writeShellScript "openclaw-wrapper" ''
        exec ${pkgs.llm-agents.openclaw}/bin/openclaw gateway
      '';
      ProtectHome = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHostname = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      RemoveIPC = true;
      RestrictSUIDSGID = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      LockPersonality = true;
      UMask = "0077";

      PrivateDevices = true;
      DeviceAllow = [
        "/dev/null rw"
        "/dev/zero rw"
        "/dev/random r"
        "/dev/urandom r"
      ];
      SystemCallFilter = [
        "@system-service"
        "~@mount"
        "@cpu-emulation"
        "@debug"
        "@keyring"
        "@module"
        "@obsolete"
        "@raw-io"
        "@reboot"
        "@swap"
      ];
      SystemCallArchitectures = "native";

      ProtectSystem = "strict";
      ReadWritePaths = [
        "/var/lib/openclaw"
      ];
      NoNewPrivileges = true;
      PrivateTmp = true;
      Restart = "always";
    };

    environment = {
      OPENCLAW_HOME = "/var/lib/openclaw";
    };
  };

  systemd.services.openclaw-browser = {
    description = "OpenClaw Browser (unrestricted)";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      WorkingDirectory = "/var/lib/openclaw";
      # NO hardening here - let Chrome do its thing
      ExecStart = "${lib.getExe pkgs.chromium} ${
        lib.escapeShellArgs [
          "--headless"
          "--no-sandbox"
          "--disable-setuid-sandbox"
          "--disable-dev-shm-usage"
          "--remote-debugging-port=9222"
          "--remote-debugging-address=127.0.0.1"
        ]
      }";
      Restart = "always";
    };
  };
}
