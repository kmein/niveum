{
  writers,
  mpv,
  dmenu,
  coreutils,
  gnused,
  di-fm-key-file,
  radioStreams,
  executableName ? "mpv-radio",
  mpvCommand ? "${mpv}/bin/mpv",
}:
let
  streams = radioStreams.override {
    di-fm-key = "%DI_FM_KEY%";
  };
in
writers.writeDashBin executableName ''
  set -x

  if [ -z ''${DI_FM_KEY} ]; then
    DI_FM_KEY=$(cat "${di-fm-key-file}")
  fi
  exec ${mpvCommand} "$(
    ${dmenu}/bin/dmenu -i -l 5 < ${streams.tsv} \
      | ${coreutils}/bin/cut -f3 \
      | ${gnused}/bin/sed s/%DI_FM_KEY%/"$DI_FM_KEY"/
  )"
''
