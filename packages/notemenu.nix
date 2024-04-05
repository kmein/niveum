{
  writers,
  lib,
  rofi,
  findutils,
  coreutils,
  noteDirectory ? "~/state/obsidian",
  currentDates ? false,
  niveumPackages,
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
    alacritty --working-directory ${noteDirectory} -e ${niveumPackages.obsidian-vim}/bin/nvim "$note_file"
  fi
''
