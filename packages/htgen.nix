{ fetchgit, lib, pkgs, stdenv }:
stdenv.mkDerivation rec {
  pname = "htgen";
  version = "1.4.0";

  src = fetchgit {
    url = "https://cgit.krebsco.de/htgen";
    rev = "refs/tags/${version}";
    sha256 = "1k6xdr4g1p2wjiyizwh33ihw3azbar7kmhyxywcq0whpip9inpmj";
  };

  installPhase = ''
    mkdir -p $out/bin
    {
      echo '#! ${pkgs.dash}/bin/dash'
      echo 'export PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.jq
        pkgs.ucspi-tcp
      ]}''${PATH+":$PATH"}'
      sed 's:^Server=htgen$:&/${version}:' htgen
    } > $out/bin/htgen
    chmod +x $out/bin/htgen
    cp -r examples $out
  '';
}
