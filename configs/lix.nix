{
  pkgs,
  lib,
  ...
}:
{
  nix = {
    package = lib.mkForce pkgs.nix;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };
}
