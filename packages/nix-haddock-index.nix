# Generate a Haddock index page for all packages visible to the current GHC
{
  writers,
  coreutils,
  gnugrep,
  gnused,
  graphviz,
  bash,
}:
writers.writeBashBin "nix-haddock-index" ''
  set -efux

  if test -z "''${NIX_GHC-}"; then
    NIX_GHC=$(${coreutils}/bin/readlink -f "$(type -P ghc)")
  fi

  if ! echo $NIX_GHC | ${gnugrep}/bin/grep -q '^/nix/store/'; then
    printf '%s: error: unsupported GHC executable path (not in Nix store): %q\n' \
        "$0" \
        "$NIX_GHC" \
      >&2
    exit 1
  fi

  NIX_GHC_PREFIX=$(${coreutils}/bin/dirname "$(${coreutils}/bin/dirname "$NIX_GHC")")
  NIX_GHC_DOCDIR=$NIX_GHC_PREFIX/share/doc/ghc/html

  main() {
    hash=$(echo $NIX_GHC_PREFIX | ${gnused}/bin/sed -n 's|^/nix/store/\([a-z0-9]\+\).*|\1|p')
    title="Haddock index for $NIX_GHC_PREFIX"

    header=$(
      printf 'Haddock index for <a href="%s">%s</a>\n' \
          $NIX_GHC_PREFIX \
          $NIX_GHC_PREFIX \
    )

    suffix=''${hash:+-$hash}
    index_file=/tmp/haddock$suffix-index.html
    svg_file=/tmp/haddock$suffix.svg

    eval "$(
      echo 'gen_index() {'
      echo '  html_head'
      "$NIX_GHC_PREFIX"/bin/ghc-pkg dump | ${gnused}/bin/sed -n '
        s/^---$/  reset/p
        s/^\(name\|version\):\s*\([-A-Za-z0-9_.]\+\)$/  \1=\2/p
        s/^haddock-html:\s*\([-A-Za-z0-9_./]\+\)$/  haddock_html \1/p
      '
      echo '  html_foot'
      echo '}'
    )"

    gen_index > $index_file

    "$NIX_GHC_PREFIX"/bin/ghc-pkg dot | ${graphviz}/bin/tred | ${graphviz}/bin/dot -Tsvg | ${gnused}/bin/sed '
      s/<svg width="[0-9]\+pt" height="[0-9]\+pt"/<svg width="3600px" height="100%"/
    ' > $svg_file

    echo $index_file
  }
  reset() {
    unset name version
  }
  haddock_html() {
    printf '<li>'
    printf '<a href="%s/index.html">%s</a>' "$1" "$name-$version"
    printf '</li>\n'
  }
  html_head() {
    printf '<!doctype html>\n'
    printf '<title>%s</title>\n' "$title"
    printf '<link href="%s" rel="stylesheet" type="text/css">\n' \
        "$NIX_GHC_DOCDIR/libraries/ocean.css"
    printf '<h1>%s</h1>\n' "$header"
    printf '<ul>\n'
  }
  html_foot() {
    printf '</ul>\n'
    printf '<a href="%s">graph</a>\n' "$svg_file"
  }

  main "$@"
''
