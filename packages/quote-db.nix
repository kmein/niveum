{ mkDerivation, base, bytestring, cassava, hasmin, HaTeX, lucid
, megaparsec, optparse-applicative, prettyprinter, raw-strings-qq
, stdenv, text
}:
mkDerivation {
  pname = "quote-db";
  version = "0.1.0.0";
  src = ./.;
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
