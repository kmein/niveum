# Recover files from a corrupted exFAT SD card
{
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
  output_dir="''${2:-$(${coreutils}/bin/mktemp -d "''${TMPDIR:-/tmp}/fix-sd-XXXXXX")}"
  mountpoint="$(${coreutils}/bin/mktemp -d "''${TMPDIR:-/tmp}/fix-sd-mount-XXXXXX")"

  trap clean EXIT
  clean() {
    ${util-linux}/bin/umount "$mountpoint" 2>/dev/null || true
    ${coreutils}/bin/rmdir "$mountpoint" 2>/dev/null || true
  }

  filenames="$(${exfatprogs}/bin/fsck.exfat "$drive" 2>&1 | ${gnused}/bin/sed -nE "s/.* file '(.*?)' is not allocated.*/\1/p")"
  ${coreutils}/bin/mkdir -p "$mountpoint" "$output_dir"
  ${util-linux}/bin/mount "$drive" "$mountpoint"

  echo "$filenames" | while read -r filename; do
    [ -n "$filename" ] || continue
    ${findutils}/bin/find "$mountpoint" -type f -name "$filename" -exec ${coreutils}/bin/cp {} "$output_dir" \;
  done

  echo "Recovered files saved to $output_dir"
  ${exfatprogs}/bin/fsck.exfat "$drive"
''
