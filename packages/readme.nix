# Render a GitHub repo's README.md as a man page
{
  lib,
  writers,
  curl,
  pandoc,
  man,
}:
writers.writeDashBin "readme" ''
  ${lib.getExe curl} -sSL "https://raw.githubusercontent.com/$*/master/README.md" \
    | ${lib.getExe pandoc} -f gfm -t man -s \
    | ${lib.getExe man} -l -
''
