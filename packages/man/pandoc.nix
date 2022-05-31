{
  stdenv,
  pandoc,
  lib,
  fetchgit,
}:
stdenv.mkDerivation {
  name = "pandoc-doc";
  version = pandoc.version;
  src = fetchgit {
    url = "https://github.com/jgm/pandoc";
    rev = pandoc.version;
    sha256 = "sha256-AY9GarrU7PBFgoYxqQkE2ghoUy7+0Gu6/Mwa+fdQM2U=";
  };
  buildPhase = ''
    mkdir -p $out/man/man1
    ${pandoc}/bin/pandoc -V section=1 --standalone --write=man $src/MANUAL.txt -o $out/man/man1/pandoc.1
  '';
}
