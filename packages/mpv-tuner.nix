{
  writeText,
  lib,
  writers,
  mpv,
  gnused,
  di-fm-key-file,
  findutils,
}: let
  streams = import ../lib/streams.nix {
    di-fm-key = "%DI_FM_KEY%";
  };
  streams-list = writeText "streams.txt" (lib.concatMapStringsSep "\n" (station: station.stream) streams);
in
  writers.writeDashBin "mpv-tuner" ''
    if [ -z ''${DI_FM_KEY} ]; then
      DI_FM_KEY=$(cat "${di-fm-key-file}")
    fi
    shuf ${streams-list} \
      | ${gnused}/bin/sed s/%DI_FM_KEY%/"$DI_FM_KEY"/ \
      | ${findutils}/bin/xargs ${mpv}/bin/mpv
  ''
