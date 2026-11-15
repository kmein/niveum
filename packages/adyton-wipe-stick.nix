# R5: overwrite a stick before it goes to adyton, so nothing old can be read
# back or hidden in; run as root on fatteh
{
  writers,
  lib,
  coreutils,
  dosfstools,
  util-linux,
}:
writers.writeBashBin "adyton-wipe-stick" ''
  set -euo pipefail
  export PATH=${
    lib.makeBinPath [
      coreutils
      dosfstools
      util-linux
    ]
  }:$PATH
  dev=''${1:?usage: adyton-wipe-stick /dev/sdX IN|OUT}
  label=''${2:?usage: adyton-wipe-stick /dev/sdX IN|OUT}
  case $label in IN | OUT) ;; *) echo "label is IN or OUT" >&2; exit 1 ;; esac
  lsblk "$dev"
  read -rp "erase $dev? type yes: " ok
  [ "$ok" = yes ]
  shred -v -n 0 -z "$dev"
  mkfs.vfat -I -n "$label" "$dev"
''
