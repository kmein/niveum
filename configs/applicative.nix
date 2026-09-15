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

  services.tailscale.enable = true;

  environment.systemPackages = [
    pkgs.ghostty
    pkgs.claude-code
    pkgs.gpu-screen-recorder-gtk
    pkgs.shotcut
    # GDM would set XDG_CURRENT_DESKTOP from the session's DesktopNames; without
    # it gnome-control-center refuses to run and portals pick the wrong backend.
    # gnome-session exports it to the user manager, so drop it again afterwards.
    (pkgs.writeShellScriptBin "gnome-session-tty" ''
      export XDG_CURRENT_DESKTOP=GNOME
      ${lib.getExe' pkgs.gnome-session "gnome-session"} "$@"
      ${lib.getExe' config.systemd.package "systemctl"} --user unset-environment XDG_CURRENT_DESKTOP
    '')
    # wdisplays (from niphas, for niri) is also called "Displays" but needs
    # wlr-output-management, which mutter lacks; keep it out of GNOME's search
    (lib.hiPrio (
      pkgs.runCommand "wdisplays-not-in-gnome" { } ''
        install -Dm644 ${pkgs.wdisplays}/share/applications/network.cycles.wdisplays.desktop -t $out/share/applications
        echo 'NotShowIn=GNOME;' >> $out/share/applications/network.cycles.wdisplays.desktop
      ''
    ))
  ];

  programs.gpu-screen-recorder.enable = true;

  # GNOME as an alternative to niri, e.g. for conference presentations: run
  # `gnome-session-tty` instead of `niri-session` from the TTY login shell
  services.desktopManager.gnome.enable = true;
  # gnome-shell runs as a user service outside the login session and only
  # accepts a logind session of a graphical type, but getty logins are "tty"
  # (and gnome-session scrubs XDG_SESSION_ID). pam_systemd takes the type from
  # XDG_SESSION_TYPE, so set it for this user's console logins; niri doesn't care.
  security.pam.services.login.rules.session =
    let
      inherit (config.security.pam.services.login.rules.session) systemd;
    in
    {
      applicative-only = {
        order = systemd.order - 20;
        control = "[success=ignore default=1]";
        modulePath = "${config.security.pam.package}/lib/security/pam_succeed_if.so";
        args = [
          "quiet"
          "user"
          "="
          config.users.users.applicative.name
        ];
      };
      wayland-session-type = {
        order = systemd.order - 10;
        control = "required";
        modulePath = "${config.security.pam.package}/lib/security/pam_env.so";
        args = [
          "conffile=${pkgs.writeText "wayland-session-type" "XDG_SESSION_TYPE DEFAULT=wayland\n"}"
          "readenv=0"
        ];
      };
    };
  # hypridle hangs off graphical-session.target (also in asg's stale
  # home-manager generation from before d65dc75d), but mutter lacks
  # ext-idle-notify, so under GNOME it restart-loops. /etc drop-ins also apply
  # to units from ~/.config; gnome-session-tty exports XDG_CURRENT_DESKTOP to
  # the user manager before the target is reached. A raw unit, since
  # systemd.user.services would also override its PATH.
  systemd.user.units."hypridle.service" = {
    overrideStrategy = "asDropin";
    text = ''
      [Unit]
      ConditionEnvironment=!XDG_CURRENT_DESKTOP=GNOME
    '';
  };
  # core apps override XDG_DATA_DIRS mimeapps (nautilus) for every session
  services.gnome.core-apps.enable = false;
  # would set GTK_IM_MODULE etc. globally, also under niri
  i18n.inputMethod.enable = false;
  # no first-login wizard or tour right before a talk
  services.gnome.gnome-initial-setup.enable = false;
  environment.gnome.excludePackages = [ pkgs.gnome-tour ];
  # only themes the GDM greeter (unused) and overlays gnome-shell, forcing a local rebuild
  stylix.targets.gnome.enable = false;
  # stylix switches Qt to the gnome platform theme as soon as GNOME is enabled
  stylix.targets.qt.platform = lib.mkForce "qtct";

  # only in this user's session; `me` keeps the stylix-generated niveum wallpaper
  niphas.wallpaper.perUser.${config.users.users.applicative.name} =
    pkgs.callPackage ../packages/applicative-wallpaper.nix
      {
        inherit (config.lib.stylix) colors;
      };

  # to run nspawn in nix sandbox
  nix.settings = {
    auto-allocate-uids = true;
    system-features = [ "uid-range" ];
    experimental-features = [
      "auto-allocate-uids"
      "cgroups"
    ];
    use-cgroups = true;
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
