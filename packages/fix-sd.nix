# Recover files from a corrupted exFAT SD card
{
  lib,
  writers,
  exfatprogs,
  util-linux,
  coreutils,
  gnused,
}:
let
  fsck = lib.getExe' exfatprogs "fsck.exfat";
in
writers.writeDashBin "fix-sd" ''
  set -efu

  drive="''${1:?Usage: fix-sd /dev/sdX [output-dir]}"
  output_dir="''${2:-$(${lib.getExe' coreutils "mktemp"} -d "''${TMPDIR:-/tmp}/fix-sd-XXXXXX")}"
  mountpoint="$(${lib.getExe' coreutils "mktemp"} -d "''${TMPDIR:-/tmp}/fix-sd-mount-XXXXXX")"

  trap clean EXIT
  clean() {
    cd /
    ${lib.getExe' util-linux "umount"} "$mountpoint" 2>/dev/null || true
    ${lib.getExe' coreutils "rmdir"} "$mountpoint" 2>/dev/null || true
  }

  # udisks auto-mounts the card on insert, but exfat-fuse and fsck -y (which
  # opens O_RDWR|O_EXCL) both need the device to themselves, and fsck -n would
  # otherwise scan a filesystem that is still changing under it
  if ${lib.getExe' util-linux "findmnt"} -S "$drive" >/dev/null; then
    echo "$drive is mounted elsewhere, unmounting ..."
    ${lib.getExe' util-linux "umount"} -A "$drive"
  fi

  # -n declines every repair, so this pass only reports. Damaged files show up
  # as "ERROR: <path>: <what> at <offset>"; ':' cannot occur in an exFAT name.
  echo "Checking $drive ..."
  corrupted="$(${fsck} -n "$drive" 2>&1 |
    ${lib.getExe gnused} -nE "s|^ERROR: (/[^:]*): .*|\1|p" |
    ${lib.getExe' coreutils "sort"} -u)"

  ${lib.getExe' coreutils "mkdir"} -p "$mountpoint" "$output_dir"
  ${lib.getExe' util-linux "mount"} -o ro "$drive" "$mountpoint"
  cd "$mountpoint"

  # save what is still readable before the repair truncates it
  printf '%s\n' "$corrupted" | while read -r path; do
    [ -n "$path" ] || continue
    ${lib.getExe' coreutils "cp"} -a --parents "''${path#/}" "$output_dir" ||
      echo "could not recover $path" >&2
  done
  echo "Recovered files saved to $output_dir"

  # only repair once nothing holds the volume any more
  clean
  ${fsck} -y "$drive" || [ $? = 1 ] # 1 means it corrected the corruption
''
