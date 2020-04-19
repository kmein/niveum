{ pkgs, lib, ... }:
let
  important-directories = pkgs.writeText "directories" ''
    h	~/
    d	~/cloud/Dropbox/
    g	~/cloud/gdrive/
    s	~/cloud/Seafile/
    kk	~/cloud/keybase/private/kmein/
    kp	~/cloud/keybase/public/kmein/
    t	/tmp
    D	~/Downloads
    cf	''${XDG_CONFIG_HOME:-$HOME/.config}
  '';
in {
  environment.systemPackages = [ pkgs.sxiv ];

  # TODO fix
  home-manager.users.me.xdg.configFile."sxiv/exec/key-handler".source = pkgs.writers.writeDash "key-handler" ''
    PATH=$PATH:${lib.makeBinPath [ pkgs.gnused pkgs.gawk pkgs.dmenu pkgs.coreutils pkgs.libnotify pkgs.imagemagick pkgs.xclip ]}

    echo >&2 key "$1" pressed
    while read file; do
        case "$1" in
        "c")
          [ -z "$destdir" ] && destdir="$(sed "s/\s.*#.*$//;/^\s*$/d" ${important-directories} | awk '{print $2}' | dmenu -l 20 -i -p "Copy file(s) to where?" | sed "s|~|$HOME|g")"
          [ -z "$destdir" ] && exit
          [ ! -d "$destdir" ] && notify-send "$destdir is not a directory, cancelled." && exit
          cp "$file" "$destdir" && notify-send -i "$(readlink -f "$file")" "$file copied to $destdir." &
          ;;
        "m")
          [ -z "$destdir" ] && destdir="$(sed "s/\s.*#.*$//;/^\s*$/d" ${important-directories} | awk '{print $2}' | dmenu -l 20 -i -p "Move file(s) to where?" | sed "s|~|$HOME|g")"
          [ -z "$destdir" ] && exit
          [ ! -d "$destdir" ] && notify-send "$destdir is not a directory, cancelled." && exit
          mv "$file" "$destdir" && notify-send -i "$(readlink -f "$file")" "$file moved to $destdir." &
          ;;
        "r")
          convert -rotate 90 "$file" "$file" ;;
        "R")
          convert -rotate -90 "$file" "$file" ;;
        "f")
          convert -flop "$file" "$file" ;;
        "y")
          echo -n "$file" | xclip -selection clipboard &&
          notify-send "$file copied to clipboard" & ;;
        "Y")
          readlink -f "$file" | xclip -selection clipboard &&
            notify-send "$(readlink -f "$file") copied to clipboard" & ;;
        "d")
          [ "$(printf "No\\nYes" | dmenu -i -p "Really delete $file?")" = "Yes" ] && rm "$file" && notify-send "$file deleted." ;;
        esac
      done
  '';
}
