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
in
{
  users.extraUsers.moodle = {
    isNormalUser = false;
    home = "/var/lib/moodle";
    createHome = true;
    openssh.authorizedKeys.keys = [
      # for sshfs mount
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDtIFmbspaBHaSkLEx43V0uaVd7l9NUFiwt2VOP++KzLjvRMLkyF2hg2HjmogTjUvTyoDs7RHwEH/cHZlJ5JQkM1jRqQpnYwGfkZEwYvmXAl6LY9+vQMP36gNPfXmKX3y3LelO7oD2uhUs0guTtg0tHUyN5/UY5u+VinyD6djxDkHaCUn3S7CS/odBcs/4flXT654wsvzgYicnSKH9R4W+7C0YsckZ/NoIkA4jnuwtWZYWrUkxd4/290buX6pAc5+zVVZqyy0sI4i8s6pO9RL5W7xvYt+w+U0u0dMxm5ckiRmLVKfIbMN4YtDxsZbZDajlQ1nDbOsEsrSXWz4H4cSNot7J820x1qh5SSxL4GSQlcT+6xCFk9kKyflxoS3oLoPLttx3rmOMkZKJWxF/IKLW47orxV6wkG5mHxdeR3cf6jX4j3nkwFVyC9R+WG4w2Z4vKONVE7uWGYU3y4OpR1e6MGHVShkNKqEvC/Kjcc/6v7I7AKRAuPZB0WEw36tA65/8= root@wilde"
    ];
    useDefaultShell = true;
  };

  system.activationScripts.moodle-dl-config = "ln -sfn ${toString moodle-dl-json} ${config.users.extraUsers.moodle.home}/config.json";

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
