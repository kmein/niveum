{ pkgs, ... }: {
  virtualisation.docker.enable = true;
  users.users.me.extraGroups = [ "docker" ];
  environment.systemPackages = [ pkgs.docker pkgs.docker_compose ];
}
