{
  lib,
  writers,
  coreutils,
  netcat,
}:
let
  inherit (lib.niveum) machines;
  sshableMachines = lib.filterAttrs (name: value: value ? "sshPort") machines;
  # Tor is handled separately: it needs a SOCKS proxy, a longer timeout, and is
  # only ever worth trying once every direct route has failed.
  directAddresses =
    system:
    # some machines carry a placeholder empty address (tabula's retiolum.ipv6)
    lib.filter (address: address != "") (
      lib.optionals (system ? "internalIp") [ system.internalIp ]
      ++ lib.optionals (system ? "externalIp") [ system.externalIp ]
      ++ lib.optionals (system ? "hyprspace" && system.hyprspace ? "ipv6") [ system.hyprspace.ipv6 ]
      ++ lib.optionals (system ? "retiolum") [
        system.retiolum.ipv6
        system.retiolum.ipv4
      ]
    );
in
lib.mapAttrs (
  name: machine:
  writers.writeBashBin "try-connect" ''
    port=${toString machine.sshPort}

    hostname=$(hostname)
    if [[ "$hostname" == "${name}" ]]; then
      echo "Target is localhost, using ::1 or 127.0.0.1" >&2
      if ${netcat}/bin/nc -z -w 2 ::1 "$port" 2>/dev/null; then
        echo "[::1]"
        exit 0
      fi
      if ${netcat}/bin/nc -z -w 2 127.0.0.1 "$port" 2>/dev/null; then
        echo "127.0.0.1"
        exit 0
      fi
    fi
    ${lib.optionalString (directAddresses machine != [ ]) ''

      # Probe every direct address at once and take the first one to answer. Done
      # sequentially, each unreachable address costs the full -w 2 before the next
      # is tried, which is most of the wait when off the LAN.
      tmp=$(${coreutils}/bin/mktemp -d)
      trap '${coreutils}/bin/rm -rf "$tmp"' EXIT
      ${coreutils}/bin/mkfifo "$tmp/winner"
      # read-write, so this open() returns immediately even though no probe has
      # opened the write end yet -- and never blocks if none ever does
      exec 3<>"$tmp/winner"

      echo "Trying ${lib.concatStringsSep ", " (directAddresses machine)}..." >&2
      for addr in ${lib.concatStringsSep " " (directAddresses machine)}; do
        # stdout goes to /dev/null rather than being inherited: a caller reading
        # us through $(...) waits for EOF on that pipe, so a still-running loser
        # would hold up the result a winner has already produced
        (${netcat}/bin/nc -z -w 2 "$addr" "$port" && echo "$addr" >&3) >/dev/null 2>&1 &
      done

      # outlasts nc's own -w 2; the timeout is what ends this when every probe fails
      if read -r -t 3 reachable <&3; then
        echo "Reached $reachable" >&2
        # IPv6 literals must be bracketed for URL authorities; note that
        # plain `ssh` accepts neither brackets nor ssh:// URIs with IPv6,
        # so ssh consumers must strip them (see niveum-ssh)
        if [[ "$reachable" == *:* ]]; then
          echo "[$reachable]"
        else
          echo "$reachable"
        fi
        # the losing probes are left to time out on their own; they hold nothing
        # the caller is waiting on
        exit 0
      fi
    ''}
    ${lib.optionalString (machine ? torAddress) ''

      if ${netcat}/bin/nc -z localhost 9050 2>/dev/null; then
        echo "Trying ${machine.torAddress} via Tor..." >&2
        if echo | ${netcat}/bin/nc -z -x localhost:9050 -w 5 ${machine.torAddress} "$port" 2>/dev/null; then
          echo "${machine.torAddress}"
          exit 0
        fi
      fi
    ''}

    echo "No reachable address found for ${name}" >&2
    exit 1
  ''
) sshableMachines
