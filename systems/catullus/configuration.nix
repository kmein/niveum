{ config, pkgs, lib, ... }:
let
  sshKey = {
    homeros = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDn13Y6CznabMvKJPIrr/dj1TX4boe8F98yc3FDElJeprQo2RXlDzjg/po9/lHTHaoC5yQUjlRg/AnI4vizYkn2sqJebAeSPahmpS+l0eFnjZgsqds2cCFqSPB6Qc5YEkGRhN4aq/ABz0jdFJLBYOYGxuuXowYxyNrqrItxDR7tF7upG+kVjYiDoP/qFm8C7zv6Zy8aoehNbzf8HlIJd0ITbMr/vUftNsQ8C84QmbZljReHmchPgE8GUfVLTlCORkhndbvNX3jXo+75y7JOIZZ6193FZHM4seg/VSDWYLJtpnhttD1w6qmiLrlimqbJB9ihoXq2eDmQ+4zo6hxQ6pFH6P0xQClJ0hxVWn6hEM3rkMwoMfbq4v54gKJsYxcGdnwjAX6d9DQv/QVjmVZffKWsGGoC7uz7bdmc0akVKi+GLSPOx8sJwXqvyvFStfqLaweVcuikUqQ72JLK7pZyliA7na6KuQ1PE3LTpfSr0lbBJ73xtS2rU1nF/Oe5zwA4LX5s/QeDVmS86D8acUrSCO62pBB3Yv8go0KR4mEvfxLiUWV6gR2uTeIPXvo4ouYFZqyABAGybjUATlGCXJaeHd/y/VWkpIB8ocqNESlRMCEe4TrYjw91AEmYBL6kWIeop3dyhovm3dTB3fQvC97kbL16wuXBrOcN4lEc+56ShhmvdQ== kieran.meinhardt@gmail.com";
    scardanelli = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC19H0FhSNWcfBRPKzbTVSMJikIWZl0CoM8zCm+/3fdMgoaLRpeZWe/AfDK6b4qOjk/sez/J0JUFCGr+JbMwjsduoazsuQowu9L9DLP9Q5UkJje4BD7MHznaeu9/XfVng/MvyaEWArA/VUJeKQesHe76tR511/+n3+bdzlIh8Zw/3wfFxmg1OTNA99/vLkXrQzHDTuV/yj1pxykL4xFtN0OIssW1IKncJeKtkO/OHGT55ypz52Daj6bNKqvxiTuzeEhv5M+5ppyIPcRf1uj/7IaPKttCgZAntEqBTIR9MbyXFeAZVayzaFnLl2okeam5XreeZbj+Y1h2ZjxiIuWoab3MLndSekVfLtfa63gtcWIf8CIvZO2wJoH8v73y0U78JsfWVaTM09ZCfFlHHA/bWqZ6laAjW+mWLO/c77DcYkB3IBzaMVNfc6mfTcGFIC+biWeYpKgA0zC6rByUPbmbIoMueP9zqJwqUaM90Nwd6559inBB107/BK3Ktb3b+37mMCstetIPB9e4EFpGMjhmnL/G81jS53ACWLXJYzt7mKU/fEsiW93MtaB+Le46OEC18y/4G8F7p/nnH7i0kO74ukxbnc4PlpiM7iWT6ra2Cyy+nzEgdXCNXywIxr05TbCQDwX6/NY8k7Hokgdfyz+1Pq3sX0yCcWRPaoB26YF12KYFQ== kieran.meinhardt@gmail.com";
  };
  unstable = import <nixos-unstable> {};
in {
  imports = [
    ./hardware-configuration.nix
    ./containers.nix
    <configs/save-space.nix>
    <configs/nixpkgs-unstable.nix>
    <modules/retiolum.nix>
    <modules/telegram-bot.nix>
    <configs/distrobump.nix>
    {
      nixpkgs.config.packageOverrides = pkgs: {
        libcoap = pkgs.callPackage <packages/libcoap.nix> {};
        traadfri =
          let traadfri-package = pkgs.fetchFromGitHub {
            owner = "kmein";
            repo = "traadfri";
            rev = "9a34ce96363e0709adf9ff842e3dfc6d469e5217";
            sha256 = "1dj4xvzq51n2s3vnwh8f83lxn00x895wc92jp83x3pkcrjvkkzxn";
          };
          in pkgs.python3Packages.callPackage traadfri-package {};
      };

      environment.systemPackages = [ pkgs.traadfri ];
      environment.variables = {
        TRAADFRI_USER = "kmein";
        TRAADFRI_HUB = "192.168.178.28";
        TRAADFRI_KEY = builtins.readFile <secrets/traadfri.key>;
      };
    }
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";

  time.timeZone = "Europe/Berlin";

  networking.wireless = {
    enable = true;
    networks.Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
  };

  environment.variables.TERM = "linux";
  environment.variables.HTOPRC = toString <dot/htoprc>;

  environment.systemPackages = with pkgs; [
    git
    vim
    htop
    wget
    reptyr
  ];

  users.mutableUsers = false;
  users.users.me = {
    name = "kfm";
    home = "/home/kfm";
    createHome = true;
    group = "users";
    hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.bash;
  };

  services.openssh = {
    enable = true;
    ports = [ 22022 ];
  };

  users.users.root.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
  ];

  users.users.me.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
  ];

  networking.retiolum = {
    ipv4 = "10.243.2.3";
    ipv6 = "42:0:3c46:3ec0:7aad:d1d5:9842:da4c";
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <secrets/retiolum.rsa_key.priv>;
    mode = "400";
  };
}
