{ pkgs, ... }:
{
  # enable `nix flake`
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
