{ pkgs, ... }:
{
  services.xserver.displayManager.sessionCommands =
  let
    intern = "LVDS-1";
    extern = "HDMI-1";
    pulseaudioCard = "alsa_card.pci-0000_00_1b.0";
    pulseaudioProfile = "output:hdmi-stereo+input:analog-stereo";
  in toString (pkgs.writers.writeDash "hdmi-on" ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output ${intern} --primary --auto --output ${extern} --above ${intern} --auto
    ${pkgs.pulseaudio}/bin/pactl set-card-profile ${pulseaudioCard} ${pulseaudioProfile}
  '');
}
