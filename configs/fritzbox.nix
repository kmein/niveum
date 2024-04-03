{ config, ... }:
{
  fileSystems."/media/fritzbox" = {
    device = "//192.168.178.1/FRITZ.NAS";
    fsType = "cifs";
    options = [
      "username=ftpuser"
      "password=ftppassword"
      "noauto"
      "uid=${toString config.users.users.me.uid}"
      "gid=${toString config.users.groups.users.gid}"
      "rw"
      "nounix"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
    ];
  };
}
