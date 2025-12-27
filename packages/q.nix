{
  writers,
  lib,
  todoman,
  khal,
  util-linux,
  wego,
  pass,
}:
writers.writeDashBin "q" ''
  export PATH=$PATH:${
    lib.makeBinPath [
      todoman
      khal
      util-linux
      wego
      pass
    ]
  }
  (todo list --due 240; echo) &
  (khal list today today; echo) &
  (cal -3; echo) &
  (wego -location Berlin -owm-api-key "$(pass api-keys/openweathermap)" -frontend emoji -days 2; echo) &
  wait
''
