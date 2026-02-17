# Recover files from a corrupted exFAT SD card
{
  lib,
  writers,
  exfatprogs,
  util-linux,
  coreutils,
  findutils,
  gnused,
}:
writers.writeDashBin "fix-sd" ''
  set -efu

  drive="''${1:?Usage: fix-sd /dev/sdX [output-dir]}"
  output_dir="''${2:-$(${lib.getExe' coreutils "mktemp"} -d "''${TMPDIR:-/tmp}/fix-sd-XXXXXX")}"
  mountpoint="$(${lib.getExe' coreutils "mktemp"} -d "''${TMPDIR:-/tmp}/fix-sd-mount-XXXXXX")"

  trap clean EXIT
  clean() {
    ${lib.getExe' util-linux "umount"} "$mountpoint" 2>/dev/null || true
    ${lib.getExe' coreutils "rmdir"} "$mountpoint" 2>/dev/null || true
  }

  filenames="$(${lib.getExe' exfatprogs "fsck.exfat"} "$drive" 2>&1 | ${lib.getExe gnused} -nE "s/.* file '(.*?)' is not allocated.*/\1/p")"
  ${lib.getExe' coreutils "mkdir"} -p "$mountpoint" "$output_dir"
  ${lib.getExe' util-linux "mount"} "$drive" "$mountpoint"

  echo "$filenames" | while read -r filename; do
    [ -n "$filename" ] || continue
    ${lib.getExe' findutils "find"} "$mountpoint" -type f -name "$filename" -exec ${lib.getExe' coreutils "cp"} {} "$output_dir" \;
  done

  echo "Recovered files saved to $output_dir"
  ${lib.getExe' exfatprogs "fsck.exfat"} "$drive"
''
