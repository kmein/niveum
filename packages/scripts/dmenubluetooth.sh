#!/bin/sh
# UI for connecting to bluetooth devices
set -efu

bluetooth_notify() {
  notify-send --app-name=" Bluetooth" "$@"
}

chose_device() {
  # the output from `bluetoothctl {paired-,}devices` has a first column which always contains `Device` followed by a MAC address and the device name
  cut -d ' ' -f2- | dmenu -i -l 5 -p "Bluetooth device"
}

bluetoothctl scan on &

case "$(printf "pair\nconnect\ndisconnect" | dmenu -i)" in
  pair)
    chosen="$(bluetoothctl devices | chose_device)"
    chosen_name="$(echo "$chosen" | cut -d ' ' -f2-)"

    bluetooth_notify "$chosen_name" "Pairing ..."

    if bluetoothctl pair "$(echo "$chosen" | cut -d ' ' -f1)"; then
      bluetooth_notify "✔ $chosen_name" "Paired with device."
    else
      test "$chosen" && bluetooth_notify "❌ $chosen_name" "Failed to pair with device."
    fi
  ;;

  connect)
    chosen="$(bluetoothctl paired-devices | chose_device)"
    chosen_name="$(echo "$chosen" | cut -d ' ' -f2-)"

    bluetooth_notify "$chosen_name" "Trying to connect ..."

    if bluetoothctl connect "$(echo "$chosen" | cut -d ' ' -f1)"; then
      bluetooth_notify "✔ $chosen_name" "Connected to device."
    else # something was selected but it didn't work
      test "$chosen" && bluetooth_notify "❌ $chosen_name" "Failed to connect to device."
    fi
  ;;

  disconnect)
    chosen="$(bluetoothctl paired-devices | chose_device)"
    chosen_name="$(echo "$chosen" | cut -d ' ' -f2-)"

    bluetooth_notify "$chosen_name" "Disconnecting ..."

    if bluetoothctl disconnect "$(echo "$chosen" | cut -d ' ' -f1)"; then
      bluetooth_notify "✔ $chosen_name" "Disconnected from device."
    else # something was selected but it didn't work
      test "$chosen" && bluetooth_notify "❌ $chosen_name" "Failed to disconnect from device."
    fi
  ;;
esac
