{ mkYarnPackage, fetchFromGitHub }:
mkYarnPackage rec {
  name = "MPD.FM";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "MPD.FM";
    rev = "9dd05f5914ece489411e4f1225444aeb18c8e9df";
    sha256 = "06m360qsx88m83bl64mpxngkqmhday80wf15y9s5w7r8lr4zyhpa";
  };
  packageJSON = "${src}/package.json";
  yarnLock = ./yarn.lock;
  yarnNix = ./yarn.nix;
}
