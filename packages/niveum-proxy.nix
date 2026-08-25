{
  symlinkJoin,
  writers,
  lib,
  netcat,
  try-connect,
}:
let
  inherit (lib.niveum) machines;
  sshableMachines = lib.filterAttrs (name: value: value ? "sshPort") machines;
in
# ssh_config ProxyCommand: pipe stdio to the first address try-connect reaches,
# so every ssh consumer (deploy-rs, scp, plain ssh via the *.niveum aliases)
# inherits the multi-route fallback without knowing about it
symlinkJoin {
  name = "niveum-proxy";
  paths = lib.mapAttrsToList (
    hostname: _:
    writers.writeBashBin "niveum-proxy-${hostname}" ''
      # $1 is ssh's %p
      reachable=$(${try-connect.${hostname}}/bin/try-connect)

      if [ -z "$reachable" ]; then
        exit 1
      fi

      # try-connect emits bracketed IPv6 literals for URL consumers; nc wants
      # them bare
      host=''${reachable#[}
      host=''${host%]}
      if [[ "$host" == *.onion ]]; then
        exec ${netcat}/bin/nc -x localhost:9050 "$host" "$1"
      else
        exec ${netcat}/bin/nc "$host" "$1"
      fi
    ''
  ) sshableMachines;
}
