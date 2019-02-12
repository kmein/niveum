{ coreutils, curl, fetchgit, gnugrep, gnused, stdenv, utillinux }:

stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/dic;
    rev = "refs/tags/v1.1.0";
    sha256 = "1xzn20b9kfz96nvjli8grpi11v80jbl0dmifksmirwcj5v81ndav";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeBinPath [
        coreutils
        curl
        gnused
        gnugrep
        utillinux
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        's,^main() {$,&\n  PATH=${path}; export PATH,' \
        < ./dic \
        > $out/bin/dic

      chmod +x $out/bin/dic
    '';
}
