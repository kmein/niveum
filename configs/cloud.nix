{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) tmpfilesConfig;
in {
  imports = [
    <niveum/modules/dropbox.nix>
  ];

  niveum = {
    dropbox.enable = false;
  };

  systemd.tmpfiles.rules = map tmpfilesConfig [
    {
      type = "L+";
      user = config.users.users.me.name;
      group = "users";
      mode = "0755";
      argument = "${config.users.users.me.home}/cloud/Seafile/Wiki";
      path = "${config.users.users.me.home}/notes";
    }
    {
      type = "L+";
      user = config.users.users.me.name;
      group = "users";
      mode = "0755";
      argument = "${config.users.users.me.home}/cloud/Seafile/Uni";
      path = "${config.users.users.me.home}/uni";
    }
    {
      type = "L+";
      user = config.users.users.me.name;
      group = "users";
      mode = "0755";
      argument = "${config.users.users.me.home}/cloud/syncthing/common/mahlzeit";
      path = "${config.users.users.me.home}/mahlzeit";
    }
  ];

  home-manager.users.me = {
    services.gnome-keyring.enable = true;
    services.nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
  };

  systemd.user.services.nextcloud-client = {
    wants = ["gnome-keyring.service"];
    after = ["gnome-keyring.service"];
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "book" ''
      set -efu
      book="$({
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/syncthing/library -type f
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/Seafile/Books -type f
      } | ${pkgs.fzf}/bin/fzf)"
      ${pkgs.zathura}/bin/zathura "$book"
    '')
  ];

  fileSystems."/media/moodle" = {
    device = "zaatar.r:/moodle";
    fsType = "nfs";
    options = [
      "x-systemd.idle-timeout=600"
      "noauto"
      "x-systemd.automount"
    ];
  };

  services.syncthing = rec {
    enable = true;
    user = "kfm";
    openDefaultPorts = true;
    configDir = "/home/kfm/.config/syncthing";
    dataDir = "/home/kfm/.config/syncthing";
    cert = toString <system-secrets/syncthing/cert.pem>;
    key = toString <system-secrets/syncthing/key.pem>;
    inherit ((import <niveum/lib>).syncthing) devices;
    folders = let
      cloud-dir = "${config.users.users.me.home}/cloud";
    in {
      "${cloud-dir}/syncthing/common".devices = ["kabsa" "manakish"];
      "${cloud-dir}/syncthing/library".devices = ["kabsa" "manakish" "heym"];
      "${cloud-dir}/syncthing/mundoiu".devices = ["kabsa" "manakish" "heym"];
      "${cloud-dir}/syncthing/music" = {
        devices = ["kabsa" "manakish" "heym" "zaatar"];
        id = "music";
      };
    };
  };
}
