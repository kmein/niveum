{ pkgs, ... }:
{
  # enable `nix flake`
  nix = {
    package = pkgs.nix;
    # extraOptions = ''
    #   experimental-features = nix-command
    # '';
  };
}
