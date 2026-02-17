# Render a GitHub repo's README.md as a man page
{
  writers,
  curl,
  pandoc,
  man,
}:
writers.writeDashBin "readme" ''
  ${curl}/bin/curl -sSL "https://raw.githubusercontent.com/$*/master/README.md" \
    | ${pandoc}/bin/pandoc -f gfm -t man -s \
    | ${man}/bin/man -l -
''
