{ config, pkgs, ... }:
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
      pkgs.chromium
      pkgs.xorg.xvfb
      pkgs.xorg.xauth
      pkgs.xorg.xkbcomp
    ];
  };

  users.groups.openclaw = { };
  users.groups.openclaw-shared = { };

  systemd.services.openclaw = {
    description = "OpenClaw Gateway Service";
    after = [
      "network.target"
      "xvfb.service"
    ];
    wantedBy = [ "multi-user.target" ];
    wants = [ "xvfb.service" ];

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
      DISPLAY = ":99";
      # tell OpenClaw where Chrome is
      PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
    };
  };

  systemd.services.xvfb = {
    description = "X Virtual Framebuffer";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      ExecStart = "${pkgs.xorg.xvfb}/bin/Xvfb :99 -screen 0 1920x1080x24 +extension GLX +render -noreset";
      Environment = "DISPLAY=:99";
    };
  };
}
