{ stdenv, dmd, curl, sqlite, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "onedrive";
  version = "v1.1.3";
  src = fetchFromGitHub {
    owner = "skilion";
    repo = "onedrive";
    rev = "945251f7f2e95ae85001efb6eab85d6176bac75e";
    sha256 = "16iajb61b09gdqly85h6h7ap385ihk0az3mimkj277yc08rv68d0";
  };
  buildInputs = [ dmd curl sqlite ];
}
