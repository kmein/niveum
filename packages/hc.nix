{
  fetchgit,
  lib,
  makeWrapper,
  stdenv,
  coreutils,
  findutils,
  gawk,
  gnugrep,
  qrencode,
  texlive,
  util-linux,
  zbar,
}:
stdenv.mkDerivation rec {
  name = "hc-${meta.version}";

  src = fetchgit {
    url = "https://cgit.krebsco.de/hc";
    rev = "refs/tags/v${meta.version}";
    sha256 = "09349gja22p0j3xs082kp0fnaaada14bafszn4r3q7rg1id2slfb";
  };

  nativeBuildInputs = [makeWrapper];

  buildPhase = null;

  installPhase = ''
    mkdir -p $out/bin

    cp $src/bin/hc $out/bin/hc

    wrapProgram $out/bin/hc \
      --prefix PATH : ${lib.makeBinPath [
      coreutils
      findutils
      gawk
      gnugrep
      qrencode
      texlive.combined.scheme-full
      util-linux
      zbar
    ]}
  '';

  meta = {
    version = "1.0.0";
  };
}
