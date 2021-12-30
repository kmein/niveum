{ pkgs, ... }:
{
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (import <nix-writers/pkgs>)
      (import <stockholm/krebs/5pkgs>)
    ];
  };
  # enable `nix flake`
  nix = {
    package = pkgs.nix;
    # extraOptions = "experimental-features = nix-command flakes";
  };
}
