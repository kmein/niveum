{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;

  moodle-dl = pkgs.callPackage <niveum/packages/moodle-dl> {};

  moodle-dl-config = {
    telegram = {
      token = lib.strings.fileContents <system-secrets/telegram/moodle-dl.token>;
      chat_id = "18980945";
      send_error_msg = true;
    };
    token = lib.strings.fileContents <system-secrets/moodle.token>;
    moodle_domain = "moodle.hu-berlin.de";
    moodle_path = "/";
    download_course_ids = [
      99881 # Dialektologie
      100183 # Onomastik
      100353 # Sanskrit I
      100692 # Sanskrit Tutorium
      99832 # Germanisch
      99823 # Gotisch
      99813 # Altalbanisch
      98681 # Geistliche Lyrik von Luther bis Lehnert
      99667 # Antike Mythologie
      52365 # FSR KlassPhil
    ];
    download_submissions = true;
    download_descriptions = false;
    download_links_in_descriptions = false;
    download_databases = false;
    download_forums = false;
    download_linked_files = false;
    download_also_with_cookie = false;
  };

  moodle-dl-json = pkgs.writeText "moodle-dl.json" (builtins.toJSON moodle-dl-config);

  moodle-dl-directory = "/var/lib/moodle";
in
{
  users.extraUsers.moodle = {
    isNormalUser = false;
    home = moodle-dl-directory;
    createHome = true;
    openssh.authorizedKeys.keys = [
      # for sshfs mount
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDtIFmbspaBHaSkLEx43V0uaVd7l9NUFiwt2VOP++KzLjvRMLkyF2hg2HjmogTjUvTyoDs7RHwEH/cHZlJ5JQkM1jRqQpnYwGfkZEwYvmXAl6LY9+vQMP36gNPfXmKX3y3LelO7oD2uhUs0guTtg0tHUyN5/UY5u+VinyD6djxDkHaCUn3S7CS/odBcs/4flXT654wsvzgYicnSKH9R4W+7C0YsckZ/NoIkA4jnuwtWZYWrUkxd4/290buX6pAc5+zVVZqyy0sI4i8s6pO9RL5W7xvYt+w+U0u0dMxm5ckiRmLVKfIbMN4YtDxsZbZDajlQ1nDbOsEsrSXWz4H4cSNot7J820x1qh5SSxL4GSQlcT+6xCFk9kKyflxoS3oLoPLttx3rmOMkZKJWxF/IKLW47orxV6wkG5mHxdeR3cf6jX4j3nkwFVyC9R+WG4w2Z4vKONVE7uWGYU3y4OpR1e6MGHVShkNKqEvC/Kjcc/6v7I7AKRAuPZB0WEw36tA65/8= root@wilde"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFFPHt+FPQ6gq8Ev65YsBZrafdtpWGpCNnlVvy67S1BK root@manakish"
    ];
    useDefaultShell = true;
  };

  system.activationScripts.moodle-dl-config = "ln -sfn ${toString moodle-dl-json} ${config.users.extraUsers.moodle.home}/config.json";

  services.syncthing = {
    enable = true;
    user = "moodle"; # config.users.extraUsers.moodle.name;
    openDefaultPorts = true;
    configDir = "${moodle-dl-directory}/.config/syncthing";
    dataDir = "${moodle-dl-directory}/.config/syncthing";
    declarative = rec {
      cert = toString <system-secrets/syncthing/cert.pem>;
      key = toString <system-secrets/syncthing/key.pem>;
      devices = {
        inherit ((import <niveum/lib>).syncthing.devices) wilde manakish toum;
      };
      folders.${moodle-dl-directory} = {
        devices = [ "toum" "wilde" "manakish" ];
        id = "moodle-dl";
      };
    };
  };

  systemd.services.moodle-dl = {
    enable = true;
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      User = config.users.extraUsers.moodle.name;
      WorkingDirectory = config.users.extraUsers.moodle.home;
    };
    wants = [ "network-online.target" ];
    script = "${moodle-dl}/bin/moodle-dl";
  };
}
