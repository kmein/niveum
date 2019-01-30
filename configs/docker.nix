{ config, pkgs, ... }:
{
  virtualisation.docker.enable = true;
  users.users.kfm.extraGroups = [ "docker" ];
}
