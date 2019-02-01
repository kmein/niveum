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
        rev = "ec4a0636c606e6e6f281df99ad06ae0582c1e292";
        sha256 = "129f5x0m4hj5i9y5bi5gjj1j5nih027kp5fv5wx9v4smbm0ph0hd";
      };
    in "${repository.out}/${pname}";

  propagatedBuildInputs = [ python-telegram-bot betacode ];
}
