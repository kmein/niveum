{ fetchFromGitHub, buildPythonApplication, buildPythonPackage, fetchPypi, aiohttp, urllib3, pillow, beautifulsoup4 }:
let
  telepot =
    buildPythonPackage rec {
      pname = "telepot";
      version = "12.7";
      src = fetchPypi {
        inherit pname version;
        sha256 = "1c587dmr71ppray0lzxgib1plnndmaiwal1kaiqx82rdwx4kw4ms";
      };
      propagatedBuildInputs = [ aiohttp urllib3 ];
      doCheck = false;
    };
in buildPythonApplication rec {
  pname = "telegram-proverb";
  version = "0.1.0";

  # src = ~/prog/git/proverb-pro;
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "proverb-pro";
    rev = "356460f5c542e8f5735c3e3eb5a356a04faf48df";
    sha256 = "02psipiyvs126b67xkqd72sfjixanrvlg02w3pn2wpc0miy5v0v3";
  };

  propagatedBuildInputs = [ telepot pillow beautifulsoup4 ];
}
