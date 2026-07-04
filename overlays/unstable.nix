# packages pulled from nixpkgs-unstable
{ nixpkgs-unstable }:
final: prev:
let
  unstablePkgs = import nixpkgs-unstable {
    inherit (prev.stdenv.hostPlatform) system;
    config = {
      allowUnfree = true;
    };
  };
in
{
  inherit (unstablePkgs)
    spotify
    ashell
    pi-coding-agent
    niri
    ;
}
