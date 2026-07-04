{
  symlinkJoin,
  writers,
  lib,
  netcat,
  openssh,
  try-connect,
}:
let
  inherit (lib.niveum) machines;
  sshableMachines = lib.filterAttrs (name: value: value ? "sshPort") machines;
in
symlinkJoin {
  name = "niveum-ssh";
  paths = lib.mapAttrsToList (
    hostname: _:
    writers.writeBashBin "niveum-ssh-${hostname}" ''
      reachable=$(${try-connect.${hostname}}/bin/try-connect)

      if [ -z "$reachable" ]; then
        exit 1
      fi

      # try-connect emits bracketed IPv6 literals for URL consumers, but ssh
      # accepts neither brackets in destinations nor IPv6 in ssh:// URIs
      host=''${reachable#[}
      host=''${host%]}
      if [[ "$host" == *.onion ]]; then
        exec ${openssh}/bin/ssh -p ${toString machines.${hostname}.sshPort} \
          -o ProxyCommand="${netcat}/bin/nc -x localhost:9050 %h %p" \
          "root@$host" "$@"
      else
        exec ${openssh}/bin/ssh -p ${toString machines.${hostname}.sshPort} \
          "root@$host" "$@"
      fi
    ''
  ) sshableMachines;
}
