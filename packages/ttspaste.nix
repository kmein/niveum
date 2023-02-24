{
  writers,
  xclip,
  curl,
  mpv,
}:
writers.writeDashBin "ttspaste" ''
  ${xclip}/bin/xclip -selection clipboard -out | ${curl}/bin/curl -G http://tts.r/api/tts --data-urlencode 'text@-' | ${mpv}/bin/mpv -
''
