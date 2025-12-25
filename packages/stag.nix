{
  lib,
  stdenv,
  fetchFromGitHub,
  ncurses,
  taglib,
  zlib,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "stag";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "smabie";
    repo = "stag";
    rev = "v${finalAttrs.version}";
    hash = "sha256-IWb6ZbPlFfEvZogPh8nMqXatrg206BTV2DYg7BMm7R4=";
  };

  buildInputs = [
    ncurses
    taglib
    zlib
  ];

  buildPhase = ''
    make all
  '';

  doCheck = true;

  installPhase = ''
    mkdir -p $out/bin
    cp stag $out/bin/

    mkdir -p $out/man/man1
    mv stag.1 $out/man/man1/
  '';
  meta = {
    description = "public domain utf8 curses based audio file tagger";
    homepage = "https://github.com/smabie/stag";
    license = lib.licenses.publicDomain;
    maintainers = [ lib.maintainers.kmein ];
    platforms = lib.platforms.unix;
  };
})
