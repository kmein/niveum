{
  writers,
  wl-clipboard,
  espeak,
}:
writers.writeDashBin "ttspaste" ''
  ${wl-clipboard}/bin/paste | ${espeak}/bin/espeak
''
# curl, mpv,
# ${curl}/bin/curl -G http://tts.r/api/tts --data-urlencode 'text@-' | ${mpv}/bin/mpv -

