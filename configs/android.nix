{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.android-tools
  ];

  users.users.me.extraGroups = [ "adbusers" ];
}
