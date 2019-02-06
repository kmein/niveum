{ fetchFromGitHub, buildPythonApplication, buildPythonPackage, fetchPypi, python-telegram-bot }:
let
  pygtrie = buildPythonPackage rec {
    pname = "pygtrie";
    version = "2.3";
    src = fetchPypi {
      inherit pname version;
      sha256 = "00x7q4p9r75zdnw3a8vd0d0w0i5l28w408g5bsfl787yv6b1h9i8";
    };
    doCheck = false;
  };
  betacode = buildPythonPackage rec {
     pname = "betacode";
     version = "0.2";
     src = fetchPypi {
       inherit pname version;
       sha256 = "08fnjzjvnm9m6p4ddyr8qgfb9bs2nipv4ls50784v0xazgxx7siv";
     };
     preBuild = ''sed -i 's/[\d128-\d255]//g' ./README.rst'';
     propagatedBuildInputs = [ pygtrie ];
     doCheck = false;
   };
in buildPythonApplication rec {
  pname = "telegram-betacode";
  version = "0.1.0";

  src =
    let
      repository = fetchFromGitHub {
        owner = "kmein";
        repo = "telebots";
        rev = "f18d856253492d268d2738616ca5176b311c1f7e";
        sha256 = "1snbb2cgr0a9cfca4l1i8xp3k5zrl4jg5s5im6grnnzzm98qacrf";
      };
    in "${repository.out}/${pname}";

  propagatedBuildInputs = [ python-telegram-bot betacode ];
}
