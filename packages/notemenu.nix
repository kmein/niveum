{
  writers,
  lib,
  rofi,
  findutils,
  coreutils,
  noteDirectory ? "~/cloud/syncthing/obsidian",
  currentDates ? false,
}:
writers.writeDashBin "notemenu" ''
  set -efu
  PATH=$PATH:${
    lib.makeBinPath [rofi findutils coreutils]
  }

  cd ${noteDirectory}
  note_file=$({
    ${lib.optionalString currentDates ''
    echo $(date -I).md
    echo $(date -I -d yesterday).md
  ''}
    find . -not -path '*/.*' -type f -printf "%T@ %p\n" | sort --reverse --numeric-sort | cut --delimiter=" " --fields=2-
  } | rofi -dmenu -i -p 'notes')
  if test "$note_file"
  then
    alacritty --working-directory ${noteDirectory} -e "$EDITOR" "$note_file"
  fi
''