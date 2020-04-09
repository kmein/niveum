{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "ix";
  src = fetchurl {
    url = "https://ix.io/client";
    sha256 = "0xc2s4s1aq143zz8lgkq5k25dpf049dw253qxiav5k7d7qvzzy57";
  };
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    install $src $out/bin/ix
  '';
}
