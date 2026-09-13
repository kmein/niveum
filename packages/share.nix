{
  lib,
  writers,
  coreutils,
  findutils,
  genpassphrase,
  jq,
  openssl,
}:
let
  inherit (lib.niveum) domain;
  base = "https://share.${domain}";
in
# Purely local: everything this writes lands in the syncthing folder, and
# syncthing is what gets it to the web host. Nothing here talks to a server.
writers.writeDashBin "share" ''
  set -efu

  export PATH=${
    lib.makeBinPath [
      coreutils
      findutils
      genpassphrase
      jq
      openssl
    ]
  }

  root=''${SHARE_DIR:-$HOME/share}
  web=$root/files
  auth=$root/auth

  usage() {
    cat >&2 <<EOF
  usage: share [-s | -p NAME] FILE...  publish files
         share -l                      list what is currently published
         share -d PATH                 unpublish PATH, e.g. public/foo.pdf

    (default)  world-readable under ${base}/public/
    -s         world-readable under an unguessable directory
    -p NAME    behind basic auth; a password is generated on first use
  EOF
    exit 1
  }

  action=put
  mode=public
  name=
  target=

  while getopts :sp:ld:h opt; do
    case $opt in
      s) mode=secret ;;
      p) mode=private; name=$OPTARG ;;
      l) action=list ;;
      d) action=delete; target=$OPTARG ;;
      *) usage ;;
    esac
  done
  shift $((OPTIND - 1))

  case "$target" in
    *..*) echo 'share: .. is not allowed in a path' >&2; exit 1 ;;
  esac
  if test "$mode" = private; then
    case "$name" in
      ""|*/*|*..*)
        echo 'share: -p takes a plain name, not a path' >&2
        exit 1
        ;;
    esac
  fi

  mkdir -p "$web/public" "$web/private" "$auth"

  case $action in
    list)
      find "$web" -mindepth 2 -type f | while IFS= read -r found; do
        printf '%s/%s\n' ${base} "$(
          printf '%s' "''${found#"$web/"}" \
            | jq -rR 'split("/") | map(@uri) | join("/")'
        )"
      done
      exit
      ;;
    delete)
      rm -rvf "$web/$target"
      exit
      ;;
  esac

  case $mode in
    public) dir=public ;;
    secret) dir=$(openssl rand -hex 8) ;;
    private)
      dir=private/$name
      # generated once and never stored anywhere else; syncthing carries the
      # htpasswd file to the web host, which is the only thing that reads it
      if ! test -s "$auth/$name"; then
        password=$(genpassphrase)
        printf '%s:%s\n' "$name" "$(openssl passwd -apr1 "$password")" > "$auth/$name"
        echo "share: created $name" >&2
        echo "  user:     $name" >&2
        echo "  password: $password" >&2
      fi
      ;;
  esac

  test $# -gt 0 || {
    test "$mode" = private || usage
    mkdir -p "$web/$dir"
    echo "${base}/$dir/"
    exit
  }

  mkdir -p "$web/$dir"
  cp -r -- "$@" "$web/$dir/"

  for file; do
    printf '%s/%s/%s\n' ${base} "$dir" \
      "$(basename "$file" | jq -rR @uri)"
  done
''
