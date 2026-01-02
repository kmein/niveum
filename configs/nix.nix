{
  pkgs,
  ...
}:
{
  nixpkgs = {
    config.allowUnfree = true;
  };
  nix = {
    package = pkgs.nixVersions.stable;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };
}
