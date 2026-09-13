{
  lib,
  writers,
  coreutils,
  genpass,
  gnused,
}:
# Correct-Horse-Battery-Staple-2026: genpass' word list, hyphenated, title-cased
# and suffixed with the current year, which is what most password fields that
# insist on a digit will accept.
writers.writeDashBin "genpassphrase" ''
  ${lib.getExe genpass} "$@" --passphrase \
    | ${lib.getExe gnused} 's/ /-/g;s/\(^\|-\)\([a-z]\)/\1\U\2/g;s/$/-'$(${lib.getExe' coreutils "date"} +%Y)'/'
''
