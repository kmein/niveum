#!/bin/sh
# UI for connecting to bluetooth devices

bluetooth_notify() {
  notify-send --app-name=" Bluetooth" "$@"
}

bluetoothctl --timeout 1 -- scan on

bluetooth_devices="$(bluetoothctl devices | cut -d ' ' -f2-)"

chosen="$(echo "$bluetooth_devices" | dmenu -i -l 5 -p "Bluetooth device")"
chosen_name="$(echo "$chosen" | cut -d ' ' -f2-)"

bluetooth_notify "$chosen_name" "Connecting ..."

if bluetoothctl connect "$(echo "$chosen" | cut -d ' ' -f1)"
then
  bluetooth_notify "✔ $chosen_name" "Connected to device."
else
  test "$chosen" && bluetooth_notify "❌ $chosen_name" "Failed to connect to device."
fi
