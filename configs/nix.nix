{
  pkgs,
  inputs,
  ...
}:
{
  nixpkgs = {
    config.allowUnfree = true;
  };
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
  };
}
