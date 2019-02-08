{ fetchFromGitHub, buildPythonApplication, pillow, python-telegram-bot, pydub, ffmpeg }:
buildPythonApplication rec {
  pname = "telegram-reverse";
  version = "0.2.0";

  src =
    let
      repository = fetchFromGitHub {
        owner = "kmein";
        repo = "telebots";
        rev = "cb248102338f2d3deac5b803d4d57ca16a047514";
        sha256 = "08xw3172zz88r9rjlb09r08h1z0nfqvi8kaqrc6lsv5ifzwyayg0";
      };
    in "${repository.out}/${pname}";

  propagatedBuildInputs = [ pillow python-telegram-bot pydub ffmpeg ];
}
