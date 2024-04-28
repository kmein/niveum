{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (import ../lib) tmpfilesConfig;
in {
  systemd.user.tmpfiles.users.me.rules = map tmpfilesConfig [
    {
      type = "d";
      mode = "0755";
      age = "7d";
      path = "${config.users.users.me.home}/sync/Downloads";
    }
  ] ++ map (path: tmpfilesConfig {
      type = "L+";
      user = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "0755";
      argument = "${config.users.users.me.home}/sync/${path}";
      path = "${config.users.users.me.home}/${path}";
  }) [".ssh" ".gnupg" ".pki" ".local/share/aerc"];

  services.gnome.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;

  home-manager.users.me = {
    services.nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
  };

  systemd.user.services.nextcloud-syncer = {
    enable = false;
    wants = ["network-online.target"];
    wantedBy = ["default.target"];
    startAt = "*:00/10";
    script = let
      kieran = {
        user = "kieran";
        passwordFile = config.age.secrets.nextcloud-password-kieran.path;
        endpoint = "https://cloud.kmein.de";
        target = "${config.users.users.me.home}/notes";
      };
    in ''
      mkdir -p ${lib.escapeShellArg kieran.target}
      ${pkgs.nextcloud-client}/bin/nextcloudcmd --non-interactive --user ${kieran.user} --password "$(cat ${kieran.passwordFile})" --path /Notes ${lib.escapeShellArg kieran.target} ${kieran.endpoint}
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
    };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "book" ''
      set -efu
      book="$({
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/syncthing/library -type f
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/nextcloud/Books -type f
      } | ${pkgs.fzf}/bin/fzf)"
      exec ${pkgs.zathura}/bin/zathura "$book"
    '')
    (let
      kieran = {
        user = "kieran.meinhardt@gmail.com";
        passwordFile = config.age.secrets.mega-password.path;
      };
      megatools = command: ''${pkgs.megatools}/bin/megatools ${command} --username ${lib.escapeShellArg kieran.user} --password "$(cat ${kieran.passwordFile})"'';
    in
      pkgs.writers.writeDashBin "book-mega" ''
        set -efu
        selection="$(${megatools "ls"} | ${pkgs.fzf}/bin/fzf)"
        test -n "$selection" || exit 1

        tmpdir="$(mktemp -d)"
        trap clean EXIT
        clean() {
          rm -rf "$tmpdir"
        }

        (
          cd "$tmpdir"
          ${megatools "get"} "$selection"
          exec ${pkgs.zathura}/bin/zathura "$(basename "$selection")"
        )
      '')
  ];

  age.secrets.mega-password = {
    file = ../secrets/mega-password.age;
    owner = config.users.users.me.name;
    group = config.users.users.me.group;
    mode = "400";
  };

  services.syncthing = {
    enable = true;
    user = "kfm";
    openDefaultPorts = true;
    configDir = "/home/kfm/.config/syncthing";
    dataDir = "/home/kfm/.config/syncthing";
    cert = config.age.secrets.syncthing-cert.path;
    key = config.age.secrets.syncthing-key.path;
    settings = {
      inherit ((import ../lib).syncthing) devices;
      folders = {
        "${config.users.users.me.home}/sync" = {
          devices = ["kabsa" "manakish" "fatteh"];
          label = "sync";
          versioning.type = "trashcan";
          versioning.params.cleanoutDays = 100;
        };
        "${config.users.users.me.home}/mobile" = {
          devices = ["kabsa" "manakish" "fatteh" "kibbeh"];
          id = "mobile";
          label = "mobile";
          versioning.type = "trashcan";
          versioning.params.cleanoutDays = 100;
        };
      };
    };
  };
}
