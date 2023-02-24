# https://www.youtube.com/watch?v=8E8sUNHdzG8
{
  writers,
  man-db,
  dmenu,
  gnused,
  findutils,
  coreutils,
  zathura,
}:
writers.writeDashBin "mansplain" ''
  ${man-db}/bin/man -k . \
    | ${coreutils}/bin/cut -d" " -f1,2 \
    | ${dmenu}/bin/dmenu -l 5 \
    | ${gnused}/bin/sed 's/\(.*\) (\(.*\))/\2 \1/' \
    | ${findutils}/bin/xargs -r ${man-db}/bin/man -t \
    | ${zathura}/bin/zathura -
''
