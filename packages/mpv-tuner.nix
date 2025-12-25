{
  writeText,
  lib,
  writers,
  mpv,
  gnused,
  di-fm-key-file,
  radioStreams,
  findutils,
}:
let
  streams = radioStreams.override {
    di-fm-key = "%DI_FM_KEY%";
  };
in
writers.writeDashBin "mpv-tuner" ''
  if [ -z ''${DI_FM_KEY} ]; then
    DI_FM_KEY=$(cat "${di-fm-key-file}")
  fi
  shuf ${streams.playlist} \
    | ${gnused}/bin/sed s/%DI_FM_KEY%/"$DI_FM_KEY"/ \
    | ${findutils}/bin/xargs ${mpv}/bin/mpv
''
