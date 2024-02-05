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
    hash = "sha256-4VDfRUr6TyF4oZsCve9t6FlEN0AqzYdlYXRny+SAcsY=";
  };
  buildPhase = ''
    mkdir -p $out/man/man1
    ${pandoc}/bin/pandoc -V section=1 --standalone --write=man $src/MANUAL.txt -o $out/man/man1/pandoc.1
  '';
}
