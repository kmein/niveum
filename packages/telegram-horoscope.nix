{ fetchFromGitHub, buildPythonApplication, buildPythonPackage, fetchPypi, aiohttp, urllib3, pytz }:
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
  pname = "telegram-horoscope";
  version = "0.1.0";

  src =
    let
      repository = fetchFromGitHub {
        owner = "kmein";
        repo = "telebots";
        rev = "702a9ac51f15419ba878862d13d09facbce729e9";
        sha256 = "11h955fc8l43w30glidqi5j9licfvp7dhrzqzgqizr0767yfn7ln";
      };
    in "${repository.out}/${pname}";

  propagatedBuildInputs = [ telepot pytz ];
}
