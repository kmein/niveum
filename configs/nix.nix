{
  pkgs,
  inputs,
  ...
}: {
  nixpkgs = {
    config.allowUnfree = true;
  };
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
  };
}
