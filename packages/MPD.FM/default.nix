{ mkYarnPackage, fetchFromGitHub }:
mkYarnPackage rec {
  name = "MPD.FM";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "MPD.FM";
    rev = "c7cbaa4ce3b350f26cad54378db22c8ec58d987b";
    sha256 = "1iklzbaji7ls01jfi1r0frhjq2i1w29kmar7vgw32f5mgj19cyvd";
  };
  packageJSON = "${src}/package.json";
  yarnLock = ./yarn.lock;
  yarnNix = ./yarn.nix;
}
