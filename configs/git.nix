{
  pkgs,
  lib,
  ...
}:
{
  home-manager.users.me = {
    programs.git = {
      enable = true;
      ignores = pkgs.lib.niveum.ignorePaths;
      settings.user.name = pkgs.lib.niveum.kieran.name;
      settings.user.email = pkgs.lib.niveum.kieran.email;
    };
  };
}
