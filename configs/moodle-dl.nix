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
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDccC/BVon+XhDLKd8XyF71WcTUcbAkFLVW95bIOtq5as2VBPTN3RvnQ1UvjfbKtzqP9zUPQ0NcqlfDK3d8GTzEpiCldBlOmb2SMR6a8Vr9dg+FtYQPnzylHJxpNjdwaapbtJwFIqF/E6SJ52L5gH7xIfCnbceZlX5qk3O1izyhcUNEvI+KyQ6nhvzGJhf8VoclwEpYK3Icu9pSEBGw7ZhCPiNNCDh8G3FQ9EVgOGKJCnZWeTlR6pvy89j6qq4AawRnHSjgOuLcACDXEbsEfI8AnGg+1+PnPv1ulqMfu9+WTi2Sv73T5ADcowfIaFI+KfM5im3IACuNnnjCdx2o9naBe23U81NxG6A6aNsLGY8q7/ibWCfXoESDuebSuWlSO25GOzraG9w/Fu7FfoR0K4hMPYl9qegSqudiYQk75cHx4VQIJuWXhiFbKdUXmwaZVdhMbRW3e/17K1pRV0H8CimRXzHiDj4QkxQL/K154QZ4I/gv32Tajb/5mhUBOXOqeps= root@homeros"
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
