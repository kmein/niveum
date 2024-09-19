{ sox, mpv, writers, coreutils }:
# ref https://askubuntu.com/a/789472
writers.writeDashBin "noise-waves" ''
  file="/tmp/noise-$(${coreutils}/bin/date +%s | ${coreutils}/bin/md5sum | ${coreutils}/bin/cut -d' ' -f1).wav"
  trap clean EXIT
  clean() {
    rm "$file"
  }

  ${coreutils}/bin/mkfifo "$file"

  ${sox}/bin/sox -c2 -r44.1k -b8 -n "$file" synth brownnoise synth pinknoise mix synth sine amod 0.3 10 &
  ${mpv}/bin/mpv "$file"
''

# play -c2 -r44.1k -b8 -n synth -1 brownnoise .1 40
# play -c2 -r44.1k -b8 -n synth -1 pinknoise .1 60
