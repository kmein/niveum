#!/bin/sh

random_mac() {
  openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//'
}

change_mac() {
  old_mac="$(ip -j link show "$interface" | jq -r '.[].address')"
  new_mac="$(random_mac)"
  ip link set "$interface" address "$new_mac" 2>/dev/null && echo "$old_mac -> $new_mac"
}

interface="${1:-wlp3s0}"
ip link set "$interface" down
until change_mac; do :; done
ip link set "$interface" up
