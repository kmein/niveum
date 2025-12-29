{
  symlinkJoin,
  writers,
  lib,
  netcat,
  openssh,
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
symlinkJoin {
  name = "niveum-ssh";
  paths = lib.mapAttrsToList (
    hostname: _:
    writers.writeBashBin "niveum-ssh-${hostname}" ''
      targets=(
        ${lib.concatStringsSep " " (map (addr: "\"root@${addr}\"") addresses.${hostname})}
      )

      for target in "''${targets[@]}"; do
        host="$(echo $target | cut -d'@' -f2)"

        # Check if it's an onion address
        if [[ "$host" == *.onion ]]; then
          # For onion addresses, try connecting through Tor
          if ${netcat}/bin/nc -z localhost 9050 2>/dev/null; then
            echo "Trying $target via Tor..." >&2
            if echo | ${netcat}/bin/nc -x localhost:9050 -w 5 "$host" ${
              toString machines.${hostname}.sshPort
            } 2>/dev/null; then
              exec ${openssh}/bin/ssh -p ${toString machines.${hostname}.sshPort} \
                -o ProxyCommand="${netcat}/bin/nc -x localhost:9050 %h %p" \
                "$target" "$@"
            fi
          fi
        else
          # For regular addresses, try direct connection
          echo "Trying $target..." >&2
          if ${netcat}/bin/nc -z -w 2 "$host" ${toString machines.${hostname}.sshPort} 2>/dev/null; then
            exec ${openssh}/bin/ssh -p ${toString machines.${hostname}.sshPort} "$target" "$@"
          fi
        fi
      done

      echo "No reachable target found for ${hostname}" >&2
      exit 1
    ''
  ) sshableMachines;
}
