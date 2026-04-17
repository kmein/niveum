let
  sshPort = 22022;
in
{
  kabsa = {
    sshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDyTnGhFq0Q+vghNhrqNrAyY+CsN7nNz8bPfiwIwNpjk";
    internalIp = "192.168.0.209";
    syncthingId = "R6DEBD7-G5RYDKN-VFA3HPO-WX4DNVI-373F7OQ-AW5MZTT-3L4BDVW-Y6ROEAF";
    retiolum = {
      ipv4 = "10.243.2.4";
      ipv6 = "42:0:3c46:861f:a118:8e9a:82c9:3d";
    };
    torAddress = "uwhxlsrkumxfjygdpoa556xs33jafcyq7gcifbdgscsoimbo5wbbksyd.onion";
    inherit sshPort;
    system = "x86_64-linux";
  };
  manakish = {
    sshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOiQEc8rTr7C7xVLYV7tQ99BDDBLrJsy5hslxtCEatkB";
    internalIp = "192.168.0.237";
    syncthingId = "AJVBWR2-VFFAGZF-7ZF5JAX-T63GMOG-NZ446WK-MC5E6WK-6X6Q2HE-QQA2JQ3";
    hyprspace = {
      id = "12D3KooWQrie3DUMzdHsCzRR2mtd7KRUsuZ8aVEYYJhnTRwHwqcA";
    };
    retiolum = {
      ipv4 = "10.243.2.85";
      ipv6 = "42:0:3c46:ac99:ae36:cb8:c551:ba27";
    };
    torAddress = "wyc6ci4obq5huqhtop2omdkqipbq2tf7bz3wqxmsnfkwwrxvaesdepad.onion";
    inherit sshPort;
    system = "x86_64-linux";
  };
  fatteh = {
    sshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIByreBjBEMJKjgpKLd5XZHIUUwIhNafVqN6OUOQpJa3y";
    internalIp = "192.168.0.59";
    syncthingId = "GSOGYT3-2GBHZXT-MNCTDIY-3BJIR4V-OHVOOMJ-ICVLKXR-U4C7RFB-HJOK3AC";
    hyprspace = {
      id = "12D3KooWDtuBQya6UnojediPEYfXjdddhoZrVLp7AszcJbrY9rdm";
      ipv4 = "100.64.40.153";
      ipv6 = "fd00:6879:7072:7370:6163:6500:140:4899";
    };
    retiolum = {
      ipv6 = "42:0:3c46:aa73:82b0:14d7:7bf8:bf2";
      ipv4 = "10.243.2.77";
    };
    torAddress = "uoe7poyeliuaudf4x5nrwvs3t55ldcdpfqfyeqsadbs77ttjx7upquyd.onion";
    inherit sshPort;
    system = "x86_64-linux";
  };
  kibbeh = {
    syncthingId = "HLQSG3D-WSKLA6S-MEYQ3EU-GDBGABE-PY53RQ6-SWQAP2I-Z5MVBVX-MYPJXAM";
  };
  ful = {
    externalIp = "130.61.217.114";
    hyprspace = {
      id = "12D3KooWLc4yK1nSxRnE7yqsZxSV7Fo7EDjyGuzA8C2AqCTXz6Ka";
      ipv4 = "100.64.239.27";
      ipv6 = "fd00:6879:7072:7370:6163:6500:fc9b:72c0";
    };
    retiolum = {
      ipv4 = "10.243.2.107";
      ipv6 = "42:0:3c46:2c8b:a564:1213:9fb4:1bc4";
    };
    torAddress = "ll3k2akcpwuo562hlbr452yvzhi6kmpjzcnjgw6z4nege2yftspgjjad.onion";
    inherit sshPort;
    system = "aarch64-linux";
  };
  zaatar = {
    internalIp = "192.168.0.47";
    hyprspace = {
      id = "12D3KooWAKu42YxewL7zP16QpVb8P8XiRDYGn5tnN7HhzfZi3UhY";
      ipv4 = "100.64.67.5";
      ipv6 = "fd00:6879:7072:7370:6163:6500:1616:3453";
    };
    retiolum = {
      ipv4 = "10.243.2.34";
      ipv6 = "42:0:3c46:156e:10b6:3bd6:6e82:b2cd";
    };
    torAddress = "hurgxlejplh7lj2hyaj4gk2fuearibst6axdxl2ekfohiivyiab3gkad.onion";
    inherit sshPort;
    system = "x86_64-linux";
  };
  makanek = {
    externalIp = "88.99.83.173";
    hyprspace = {
      id = "12D3KooWQtzDBoCoTh64U5pDP58Krah7bAY1CWcdzWjcUKhPJS3b";
      ipv4 = "100.64.31.124";
      ipv6 = "fd00:6879:7072:7370:6163:6500:b628:c814";
    };
    retiolum = {
      ipv4 = "10.243.2.84";
      ipv6 = "42:0:3c46:f7a9:1f0a:1b2b:822a:6050";
    };
    torAddress = "gnaoacvkhovpllpiwi4a4mbnx4awpdcufwtsj365tiweybdeec7thuyd.onion";
    inherit sshPort;
    system = "x86_64-linux";
  };
  officejet = {
    internalIp = "192.168.0.251";
  };
  router = {
    internalIp = "192.168.0.1";
  };
  tabula = {
    retiolum = {
      ipv4 = "10.243.2.78";
      ipv6 = "";
    };
    inherit sshPort;
    system = "x86_64-linux";
  };
  tahina = {
    retiolum = {
      ipv4 = "10.243.2.74";
      ipv6 = "42:0:3c46:2923:1c90:872:edd6:306";
    };
    inherit sshPort;
    system = "x86_64-linux";
  };
}
