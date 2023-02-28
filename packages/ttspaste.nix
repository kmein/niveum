{
  writers,
  xclip,
  espeak,
}:
writers.writeDashBin "ttspaste" ''
  ${xclip}/bin/xclip -selection clipboard -out | ${espeak}/bin/espeak
''
# curl, mpv,
# ${curl}/bin/curl -G http://tts.r/api/tts --data-urlencode 'text@-' | ${mpv}/bin/mpv -

