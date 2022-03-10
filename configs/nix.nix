{pkgs, ...}: {
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (import <nix-writers/pkgs>)
      (import <stockholm/krebs/5pkgs>)
    ];
  };
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
