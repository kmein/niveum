{ mkDerivation, base, fetchgit, hspec, QuickCheck, stdenv, text }:
mkDerivation {
  pname = "blessings";
  version = "2.2.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/blessings";
    sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
    rev = "d94712a015636efe7ec79bc0a2eec6739d0be779";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.mit;
}
