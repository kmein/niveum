{
  writers,
  bc,
  coreutils,
  espeak,
}:
writers.writeDashBin "timer" ''
  [ $# -eq 2 ] || {
    echo "Usage: $0 TIME MESSAGE" 1>&2
    exit 1
  }
  echo "sleeping $time seconds, then saying: $2"
  ${coreutils}/bin/sleep "$time" && ${espeak}/bin/espeak -v german-mbrola-6 "$2"
''
