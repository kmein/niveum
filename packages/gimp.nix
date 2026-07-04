{
  gimp,
  runCommand,
  symlinkJoin,
  writers,
}:
let
  bring-out-the-gimp = ./bring-out-the-gimp.png;
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
