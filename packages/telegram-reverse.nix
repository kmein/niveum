{ buildPythonPackage, pillow, python-telegram-bot, pydub, ffmpeg }:
buildPythonPackage rec {
  pname = "telegram-reverse";
  version = "0.2.0";

  src = "${builtins.fetchTarball https://github.com/kmein/telebots/archive/e83ec7d78f24214801d53cc3706918d282d9cadf.tar.gz}/${pname}";

  propagatedBuildInputs = [ pillow python-telegram-bot pydub ffmpeg ];
}
