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
  time=$(echo "$1" | ${bc}/bin/bc)
  echo "sleeping $time seconds, then saying: $2"
  ${coreutils}/bin/sleep "$time" && {
    echo "$2" | ${espeak}/bin/espeak -v german-mbrola-6
  }
''
