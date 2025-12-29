{
  gimp,
  fetchurl,
  runCommand,
  symlinkJoin,
  writers,
}:
let
  bring-out-the-gimp = fetchurl {
    url = "https://c.krebsco.de/bring-out-the-gimp.png";
    hash = "sha256-k42M5j58OzbcMJwx9CeM2mD2VsVwPKHwJvU55+Rkowc=";
  };
  data-dir-prefix = "share/gimp/3.0";
  data-dir = symlinkJoin {
    name = "gimp";
    paths = [
      (runCommand "splash" { } ''
        mkdir -p $out/${data-dir-prefix}/images
        install ${bring-out-the-gimp} ${placeholder "out"}/share/gimp/3.0/images/gimp-splash.png
      '')
      gimp
    ];
  };
in
writers.writeDashBin "gimp" ''
  exec env GIMP3_DATADIR=${data-dir}/${data-dir-prefix} ${gimp}/bin/gimp "$@"
''
