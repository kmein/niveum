{ stdenv, pandoc, lib, fetchgit }:
stdenv.mkDerivation {
  name = "pandoc-doc";
  version = pandoc.version;
  src = fetchgit {
    url = "https://github.com/jgm/pandoc";
    rev = pandoc.version;
    sha256 = "0s4mczbql35wh6bhyi542yln24f530rlsw6akcv7lmp083rrlpy4";
  };
  buildPhase = ''
    mkdir -p $out/man/man1
    ${pandoc}/bin/pandoc -V section=1 --standalone --write=man $src/MANUAL.txt -o $out/man/man1/pandoc.1
  '';
}
