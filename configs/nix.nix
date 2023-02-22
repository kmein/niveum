{pkgs, ...}: {
  nixpkgs = {
    config.allowUnfree = true;
  };
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
