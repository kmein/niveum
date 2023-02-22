{
  fetchgit,
  lib,
  stdenv,
  coreutils,
  curl,
  gnugrep,
  gnused,
  util-linux,
}:
stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = https://cgit.ni.krebsco.de/dic;
    rev = "refs/tags/v1.1.1";
    sha256 = "1gbj967a5hj53fdkkxijqgwnl9hb8kskz0cmpjq7v65ffz3v6vag";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = let
    path = lib.makeBinPath [
      coreutils
      curl
      gnused
      gnugrep
      util-linux
    ];
  in ''
    mkdir -p $out/bin

    sed \
      's,^main() {$,&\n  PATH=${path}; export PATH,' \
      < ./dic \
      > $out/bin/dic

    chmod +x $out/bin/dic
  '';
}
