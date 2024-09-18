{ sox, mpv, writers, coreutils }:
# ref https://askubuntu.com/a/789472
writers.writeDashBin "noise-waves" ''
  file="/tmp/noise.wav"
  trap clean EXIT
  clean() {
    rm "$file"
  }

  ${coreutils}/bin/mkfifo "$file"

  ${sox}/bin/sox -n "$file" synth brownnoise synth pinknoise mix synth sine amod 0.3 10 &
  ${mpv}/bin/mpv "$file"
''
