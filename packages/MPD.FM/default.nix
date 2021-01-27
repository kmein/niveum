{ mkYarnPackage, fetchFromGitHub }:
mkYarnPackage rec {
  name = "MPD.FM";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "MPD.FM";
    rev = "5f309c2579a9cbbbc4f7eb6a2e2f3993cc177630";
    sha256 = "0ladh96s656i7yd9qxrpqq4x513r88zas7112rqn5sgxxaccbh72";
  };
  packageJSON = "${src}/package.json";
  yarnLock = ./yarn.lock;
  yarnNix = ./yarn.nix;
}
