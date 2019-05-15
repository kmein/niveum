{ mkDerivation, base, bytestring, cassava, hasmin, HaTeX, lucid
, megaparsec, optparse-applicative, prettyprinter, raw-strings-qq
, stdenv, text
, fetchFromGitHub
}:
mkDerivation {
  pname = "quote-db";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "quote-db";
    rev = "2a9776293d306cf2777bfd63412d8e30082826d1";
    sha256 = "18nmi6rds1dfwkrnvdbqzl261szg93b6rbx6in2zmpz8v6k6z5f9";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava hasmin HaTeX lucid megaparsec prettyprinter
    raw-strings-qq text
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/kmein/quote-db#readme";
  description = "A tool for managing a database of literature quotes";
  license = stdenv.lib.licenses.mit;
}
