{ stdenv, lib, python3Packages }:
python3Packages.buildPythonApplication rec {
  pname = "moodle-dl";
  version = "2.1.1.1";

  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "0nv2gm4x6mfsa0wywm4xbsx6nf6hx4kb63s4lczaxb9m4ybjd45h";
  };

  patches = [
    ./readchar-version.patch
    ./telegram-format.patch
  ];

  propagatedBuildInputs = with python3Packages; [
    sentry-sdk
    colorama
    readchar
    youtube-dl
    certifi
    html2text
    requests
  ];

  meta = with stdenv.lib; {
    homepage = "https://github.com/C0D3D3V/Moodle-Downloader-2";
    maintainers = [ maintainers.kmein ];
    description = "A Moodle downloader that downloads course content fast from Moodle";
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
