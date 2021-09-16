{ lib, pkgs, ... }: {
  virtualisation.docker = {
    enable = true;
    # for ICE wifi, ref https://gist.github.com/sunsided/7840e89ff4e11b64a2d7503fafa0290c
    extraOptions = lib.concatStringsSep " " [
      "--bip=172.39.1.5/24"
      "--fixed-cidr=172.39.1.0/25"
    ];
  };
  users.users.me.extraGroups = [ "docker" ];
  environment.systemPackages = [ pkgs.docker pkgs.docker_compose ];
}
