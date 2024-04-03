{ config, ... }:
{
  fileSystems."/media/fritz" = {
    device = "//192.168.178.1/FRITZ.NAS";
    fsType = "cifs";
    options = [
      "username=ftpuser"
      "password=ftppassword"
      "noauto"
      "uid=${toString config.users.users.me.uid}"
      "gid=${toString config.users.groups.users.gid}"
      "workgroup=WORKGROUP"
      "rw"
      "noserverino" # ref https://askubuntu.com/a/1265165
      "nounix"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
    ];
  };
}
