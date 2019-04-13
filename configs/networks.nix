{ pkgs, config, lib, ... }:
let
  sshPort = 22022;
  sshKey = {
    homeros = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDn13Y6CznabMvKJPIrr/dj1TX4boe8F98yc3FDElJeprQo2RXlDzjg/po9/lHTHaoC5yQUjlRg/AnI4vizYkn2sqJebAeSPahmpS+l0eFnjZgsqds2cCFqSPB6Qc5YEkGRhN4aq/ABz0jdFJLBYOYGxuuXowYxyNrqrItxDR7tF7upG+kVjYiDoP/qFm8C7zv6Zy8aoehNbzf8HlIJd0ITbMr/vUftNsQ8C84QmbZljReHmchPgE8GUfVLTlCORkhndbvNX3jXo+75y7JOIZZ6193FZHM4seg/VSDWYLJtpnhttD1w6qmiLrlimqbJB9ihoXq2eDmQ+4zo6hxQ6pFH6P0xQClJ0hxVWn6hEM3rkMwoMfbq4v54gKJsYxcGdnwjAX6d9DQv/QVjmVZffKWsGGoC7uz7bdmc0akVKi+GLSPOx8sJwXqvyvFStfqLaweVcuikUqQ72JLK7pZyliA7na6KuQ1PE3LTpfSr0lbBJ73xtS2rU1nF/Oe5zwA4LX5s/QeDVmS86D8acUrSCO62pBB3Yv8go0KR4mEvfxLiUWV6gR2uTeIPXvo4ouYFZqyABAGybjUATlGCXJaeHd/y/VWkpIB8ocqNESlRMCEe4TrYjw91AEmYBL6kWIeop3dyhovm3dTB3fQvC97kbL16wuXBrOcN4lEc+56ShhmvdQ== kieran.meinhardt@gmail.com";
    scardanelli = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC19H0FhSNWcfBRPKzbTVSMJikIWZl0CoM8zCm+/3fdMgoaLRpeZWe/AfDK6b4qOjk/sez/J0JUFCGr+JbMwjsduoazsuQowu9L9DLP9Q5UkJje4BD7MHznaeu9/XfVng/MvyaEWArA/VUJeKQesHe76tR511/+n3+bdzlIh8Zw/3wfFxmg1OTNA99/vLkXrQzHDTuV/yj1pxykL4xFtN0OIssW1IKncJeKtkO/OHGT55ypz52Daj6bNKqvxiTuzeEhv5M+5ppyIPcRf1uj/7IaPKttCgZAntEqBTIR9MbyXFeAZVayzaFnLl2okeam5XreeZbj+Y1h2ZjxiIuWoab3MLndSekVfLtfa63gtcWIf8CIvZO2wJoH8v73y0U78JsfWVaTM09ZCfFlHHA/bWqZ6laAjW+mWLO/c77DcYkB3IBzaMVNfc6mfTcGFIC+biWeYpKgA0zC6rByUPbmbIoMueP9zqJwqUaM90Nwd6559inBB107/BK3Ktb3b+37mMCstetIPB9e4EFpGMjhmnL/G81jS53ACWLXJYzt7mKU/fEsiW93MtaB+Le46OEC18y/4G8F7p/nnH7i0kO74ukxbnc4PlpiM7iWT6ra2Cyy+nzEgdXCNXywIxr05TbCQDwX6/NY8k7Hokgdfyz+1Pq3sX0yCcWRPaoB26YF12KYFQ== kieran.meinhardt@gmail.com";
  };
in {
  networking.hosts = lib.mapAttrs (_: value: [ (value + ".local") ]) {
    "192.168.178.1" = "router";
    "192.168.178.21" = "scardanelli";
    "192.168.178.22" = "homeros";
    "192.168.178.24" = "catullus";
    "192.168.178.27" = "printer";
  };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      Aether.pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
      EasyBox-927376.pskRaw = "dbd490ab69b39bd67cfa06daf70fc3ef3ee90f482972a668ed758f90f5577c22";
      "Asoziales Netzwerk".pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6";
      c-base-public = {};
      security-by-obscurity.psk = "44629828256481964386";
      discord.psk = "baraustrinken";
    };
  };

  # for kdeconnect
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  home-manager.users.kfm = {
    services.kdeconnect = {
      enable = true;
      indicator = true;
    };

    programs.ssh = {
      enable = true;
      matchBlocks = {
        scardanelli = {
          hostname = "scardanelli.local";
          user = "kfm";
          port = sshPort;
        };
        homeros = {
          hostname = "homeros.local";
          user = "kfm";
          port = sshPort;
        };
        catullus = {
          hostname = "catullus.local";
          user = "root";
        };
        menstruation-logs = {
          user = "root";
          hostname = "v22017123717457389.megasrv.de";
          port = 49123;
        };
        irc = {
          user = "kmein";
          hostname = "prism.r";
          extraOptions.RemoteCommand = "tmux a";
        };
        "gitlab.peixdev.net".port = 999;
      };
    };
  };

  programs.ssh.startAgent = true;

  services.openssh = {
    ports = [ sshPort ];
    enable = true;
    forwardX11 = true;
    passwordAuthentication = false;
  };

  services.syncthing = rec {
    enable = true;
    # user = config.users.users.kfm.name;
    # dataDir = "${config.users.users.kfm.home}/.config/syncthing";
    user = "kfm";
    dataDir = "/home/${user}/.config/syncthing";
    openDefaultPorts = true;
  };

  users.users.root.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
  ];

  users.users.kfm.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
  ];
}
