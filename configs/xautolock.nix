{ config, pkgs, ... }:
{
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.systemd}/bin/systemctl --user import-environment XDG_SESSION_PATH
    ${pkgs.lightlocker}/bin/light-locker &
  '';

  services.xserver.xautolock = rec {
    enable = true;
    killer = "${pkgs.systemd}/bin/systemctl suspend";
    locker = "${pkgs.i3lock}/bin/i3lock";
    nowlocker = locker;
    enableNotifier = true;
    notifier = ''${pkgs.libnotify}/bin/notify-send -u normal -a xautolock "Locking soon" "The screen will lock in 10 seconds."'';
  };
}
