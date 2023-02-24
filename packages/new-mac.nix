{
  openssl,
  writers,
  gnused,
  iproute2,
  jq,
}:
writers.writeDashBin "new-mac" ''
  random_mac() {
    ${openssl}/bin/openssl rand -hex 6 | ${gnused}/bin/sed 's/\(..\)/\1:/g; s/.$//'
  }

  change_mac() {
    old_mac="$(${iproute2}/bin/ip -j link show "$interface" | ${jq}/bin/jq -r '.[].address')"
    new_mac="$(random_mac)"
    ${iproute2}/bin/ip link set "$interface" address "$new_mac" 2>/dev/null && echo "$old_mac -> $new_mac"
  }

  interface="''${1:-wlp3s0}"
  ${iproute2}/bin/ip link set "$interface" down
  until change_mac; do :; done
  ${iproute2}/bin/ip link set "$interface" up
''
