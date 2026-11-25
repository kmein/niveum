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
    ;
  # hebrew voices need piper 1.5; its http server runs as a module, so with the python it was built for
  piper-tts-python = unstablePkgs.python3.withPackages (ps: [ (ps.toPythonModule unstablePkgs.piper-tts) ]);
}
