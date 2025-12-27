{
  stdenv,
  lib,
  fetchurl,
  weechat,
}:
stdenv.mkDerivation {
  pname = "hotlist2extern";
  version = "1.0";

  src = fetchurl {
    url = "https://raw.githubusercontent.com/weechat/scripts/dd627975cf2e464f206f8006cb3963c8ee82044c/perl/hotlist2extern.pl";
    sha256 = "1flpikm1kq6m9rh3hmafni9f2yi1b90w539k3hj55a5c9gddp2lr";
  };

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out/share
    cp $src $out/share/hotlist2extern.pl
  '';

  passthru = {
    scripts = [ "hotlist2extern.pl" ];
  };

  meta = with lib; {
    inherit (weechat.meta) platforms;
    description = "Give hotlist to an external file/program";
    license = licenses.gpl3;
    maintainers = with maintainers; [ kmein ];
  };
}
