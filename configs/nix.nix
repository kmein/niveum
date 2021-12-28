{ pkgs, ... }:
{
  # enable `nix flake`
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
