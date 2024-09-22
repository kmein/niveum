{ buildPythonPackage, lib, fetchPypi, gitpython, gensim, tqdm, torch, stringcase, stanza, spacy, scipy, scikit-learn, requests, rapidfuzz, pyyaml, nltk, boltons, poetry-core }:
let
  greek-accentuation = buildPythonPackage rec {
    pname = "greek-accentuation";
    version = "1.2.0";
    src = fetchPypi {
      inherit pname version;
      hash = "sha256-l2HZXdqlLubvy2bWhhZVYGMpF0DXVKTDFehkcGF5xdk=";
    };
    meta = with lib; {
      description = "Python 3 library for accenting (and analyzing the accentuation of) Ancient Greek words";
      homepage = "https://github.com/jtauber/greek-accentuation";
      license = licenses.mit;
      maintainers = with maintainers; [ kmein ];
    };
  };
in
buildPythonPackage rec {
  pname = "cltk";
  format = "pyproject";
  version = "1.3.0";
  src = fetchPypi {
    inherit pname version;
    hash = "sha256-jAxvToUIo333HSVQDYVyUBY3YP+m1RnlNGelcvktp6s=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace "poetry>=1.1.13" poetry-core \
      --replace "poetry.masonry.api" "poetry.core.masonry.api" \
      --replace 'scipy = "<1.13.0"' 'scipy = "^1"' \
      --replace 'boltons = "^21.0.0"' 'boltons = "^24.0.0"'
  '';

  propagatedBuildInputs = [
    gitpython
    gensim
    boltons
    greek-accentuation
    pyyaml
    nltk
    rapidfuzz
    requests
    scikit-learn
    scipy
    spacy
    stanza
    stringcase
    torch
    tqdm
  ];

  nativeBuildInputs = [ poetry-core ];

  meta = with lib; {
    description = "Natural language processing (NLP) framework for pre-modern languages";
    homepage = "https://cltk.org";
    license = licenses.mit;
    maintainers = with maintainers; [ kmein ];
  };
}
