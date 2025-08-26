{ writers, wofi, pass, libnotify, ... }:
writers.writeBashBin "passmenu" ''
  shopt -s nullglob globstar

  IFS=$'\n'

  prefix=$(readlink -f $${PASSWORD_STORE_DIR-~/.password-store})
  password_files=( $( fd -L ".gpg\$" "$prefix" ) )
  password_files=( "$${password_files[@]#"$prefix"/}" )
  password_files=( "$${password_files[@]%.gpg}" )

  password=$( printf '%s\n' "$${password_files[@]}" | ${wofi}/bin/wofi -i -k /dev/null -d menu -- "$@" )

  [[ -n $password ]] || exit

  OUT=$(${pass}/bin/pass show --clip "$password")
  ${libnotify}/bin/notify-send -t 5000 "$(echo "$OUT" | grep '^login:' | sed 's/^login: //')"
''
