{
  lib,
  writers,
  netcat,
}:
let
  inherit (lib.niveum) machines;
  sshableMachines = lib.filterAttrs (name: value: value ? "sshPort") machines;
  systemAddresses =
    system:
    lib.optionals (system ? "internalIp") [ system.internalIp ]
    ++ lib.optionals (system ? "externalIp") [ system.externalIp ]
    ++ lib.optionals (system ? "retiolum") [
      system.retiolum.ipv6
      system.retiolum.ipv4
    ]
    ++ lib.optionals (system ? "mycelium") [ system.mycelium.ipv6 ]
    ++ lib.optionals (system ? "torAddress") [ system.torAddress ];
  addresses = lib.listToAttrs (
    map (name: {
      inherit name;
      value = systemAddresses (machines.${name});
    }) (builtins.attrNames sshableMachines)
  );
in
lib.mapAttrs (
  name: _:
  writers.writeBashBin "try-connect" ''
    port=${toString machines.${name}.sshPort}

    for addr in ${lib.concatStringsSep " " addresses.${name}}; do
      # Check if it's an onion address
      if [[ "$addr" == *.onion ]]; then
        if ${netcat}/bin/nc -z localhost 9050 2>/dev/null; then
          echo "Trying $addr via Tor..." >&2
          if echo | ${netcat}/bin/nc -z -x localhost:9050 -w 5 "$addr" "$port" 2>/dev/null; then
            echo "$addr"
            exit 0
          fi
        fi
      else
        echo "Trying $addr..." >&2
        if ${netcat}/bin/nc -z -w 2 "$addr" "$port" 2>/dev/null; then
          echo "$addr"
          exit 0
        fi
      fi
    done

    echo "No reachable address found for ${name}" >&2
    exit 1
  ''
) sshableMachines
