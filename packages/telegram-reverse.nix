{ fetchFromGitHub, buildPythonApplication, pillow, python-telegram-bot, pydub, ffmpeg }:
buildPythonApplication rec {
  pname = "telegram-reverse";
  version = "0.2.0";

  src =
    let
      repository = fetchFromGitHub {
        owner = "kmein";
        repo = "telebots";
        rev = "ec4a0636c606e6e6f281df99ad06ae0582c1e292";
        sha256 = "129f5x0m4hj5i9y5bi5gjj1j5nih027kp5fv5wx9v4smbm0ph0hd";
      };
    in "${repository.out}/${pname}";

  propagatedBuildInputs = [ pillow python-telegram-bot pydub ffmpeg ];
}
