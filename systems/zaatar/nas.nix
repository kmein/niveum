{ config, ... }:
{
  users.extraUsers.nas = {
    isSystemUser = true;
    group = "nas";
    uid = 7451;
  };
  users.extraGroups.nas = {
    gid = 7452;
  };

  fileSystems."/nas" = {
    device = "/dev/disk/by-id/0x50014ee658872039-part1";
    fsType = "ntfs";
    options = [ # ref https://askubuntu.com/a/113746
      "defaults"
      "nls=utf8"
      "umask=000"
      "dmask=027"
      "fmask=137"
      "uid=${toString config.users.extraUsers.nas.uid}"
      "gid=${toString config.users.extraGroups.nas.gid}"
      "windows_names"
    ];
  };

  # ref https://dataswamp.org/~solene/2020-10-18-nixos-nas.html
  # ref https://www.reddit.com/r/NixOS/comments/relwsh/comment/hoapgrr/
  services.samba = {
    enable = true;
    securityType = "user";
    openFirewall = true;
    extraConfig = ''
      workgroup = WORKGROUP
      server string = zaatar
      server role = standalone server
      netbios name = zaatar
      security = user
      hosts allow = 192.168.178. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = Bad User
    '';
    shares.nas = {
      path = "/nas";
      browseable = "yes";
      writable = "yes";
      # "read only" = "no";
      "guest ok" = "yes";
      "create mask" = "0644";
      "directory mask" = "0755";
      "force user" = config.users.extraUsers.nas.name;
      "force group" = config.users.extraUsers.nas.group;
    };
  };

  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
  };

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
}