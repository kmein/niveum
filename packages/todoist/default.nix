{ stdenv, go, buildGoPackage, fetchFromGitHub }:
buildGoPackage rec {
  name = "todoist-${version}";
  version = "0.13.0";
  src = fetchFromGitHub {
    owner = "sachaos";
    repo = "todoist";
    rev = "4db9c55408d408ce8385ac6e845b83f320c45c28";
    sha256 = "1697mfq7ajr938ps387hj689k7s93274xlfyns1lr4gw1mfpxhzc";
  };
  goDeps = ./deps.nix;
  goPackagePath = "github.com/sachaos/todoist";
}
