{
  fetchgit,
  lib,
  pkgs,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "htgen";
  version = "1.3.1";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/${version}";
    sha256 = "0ml8kp89bwkrwy6iqclzyhxgv2qn9dcpwaafbmsr4mgcl70zx22r";
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
