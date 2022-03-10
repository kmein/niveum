{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.niveum.minecraft;
in {
  options.niveum.minecraft = {enable = mkEnableOption "Minecraft";};

  config = mkIf cfg.enable {environment.systemPackages = [pkgs.minecraft];};
}
