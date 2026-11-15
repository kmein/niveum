# R2: run on fatteh; builds adyton here and on a second builder, and signs
# the closure onto the IN stick only if both builds agree
{
  writers,
  lib,
  nix,
  jq,
  diffutils,
}:
writers.writeBashBin "adyton-publish" ''
  set -euo pipefail
  export PATH=${
    lib.makeBinPath [
      nix
      jq
      diffutils
    ]
  }:$PATH
  if [ $# -ne 3 ]; then
    echo "usage: adyton-publish <IN stick> <update secret key> <second builder, e.g. ssh-ng://root@ful>" >&2
    exit 1
  fi
  in=$1 key=$2 remote=$3

  drv=$(nix eval --raw '.?submodules=1#nixosConfigurations.adyton.config.system.build.toplevel.drvPath')
  system=$(nix build --no-link --print-out-paths "$drv^out")
  nix build --no-link --eval-store auto --store "$remote" "$drv^out"

  # path-info --json is an array in older nix, an object keyed by path in newer
  hashes() {
    nix path-info --json --recursive "$@" "$system" |
      jq -S 'if type == "array" then map({ key: .path, value: .narHash }) | from_entries else map_values(.narHash) end'
  }
  if ! diff <(hashes) <(hashes --store "$remote"); then
    echo "the two builds differ; nothing signed" >&2
    exit 1
  fi

  nix copy --to "file://$in/cache?compression=zstd&secret-key=$key" "$system"
  printf '%s\n' "$system" > "$in/system"
  echo "signed $system onto $in"
''
