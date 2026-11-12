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
    niri
    # from 0.13 on, iamb trusts the system store and with it ca.r (chat.kmein.r)
    iamb
    ;
}
