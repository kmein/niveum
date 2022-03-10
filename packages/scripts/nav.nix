{
  lib,
  fetchFromGitHub,
  stdenv,
  fzf,
}:
stdenv.mkDerivation rec {
  name = "nav";
  version = "8da22b1";
  src = fetchFromGitHub {
    owner = "huntrar";
    repo = name;
    rev = version;
    sha256 = "0aw10495901dagyfxn2pj6nh3nl1xgi5p57mwgkgn9g5hi66xa87";
  };
  installPhase = ''
    mkdir -p $out/bin/
    install nav $out/bin/
  '';
  propagatedBuildInputs = [fzf];
  meta = {
    maintainer = [lib.maintainers.kmein];
    description = "JSON data structure navigator";
    homepage = "https://github.com/huntrar/nav";
  };
}
