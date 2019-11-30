{ config, pkgs, lib, ... }:
let
  sshKey = {
    homeros = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDn13Y6CznabMvKJPIrr/dj1TX4boe8F98yc3FDElJeprQo2RXlDzjg/po9/lHTHaoC5yQUjlRg/AnI4vizYkn2sqJebAeSPahmpS+l0eFnjZgsqds2cCFqSPB6Qc5YEkGRhN4aq/ABz0jdFJLBYOYGxuuXowYxyNrqrItxDR7tF7upG+kVjYiDoP/qFm8C7zv6Zy8aoehNbzf8HlIJd0ITbMr/vUftNsQ8C84QmbZljReHmchPgE8GUfVLTlCORkhndbvNX3jXo+75y7JOIZZ6193FZHM4seg/VSDWYLJtpnhttD1w6qmiLrlimqbJB9ihoXq2eDmQ+4zo6hxQ6pFH6P0xQClJ0hxVWn6hEM3rkMwoMfbq4v54gKJsYxcGdnwjAX6d9DQv/QVjmVZffKWsGGoC7uz7bdmc0akVKi+GLSPOx8sJwXqvyvFStfqLaweVcuikUqQ72JLK7pZyliA7na6KuQ1PE3LTpfSr0lbBJ73xtS2rU1nF/Oe5zwA4LX5s/QeDVmS86D8acUrSCO62pBB3Yv8go0KR4mEvfxLiUWV6gR2uTeIPXvo4ouYFZqyABAGybjUATlGCXJaeHd/y/VWkpIB8ocqNESlRMCEe4TrYjw91AEmYBL6kWIeop3dyhovm3dTB3fQvC97kbL16wuXBrOcN4lEc+56ShhmvdQ== kieran.meinhardt@gmail.com";
    scardanelli = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC19H0FhSNWcfBRPKzbTVSMJikIWZl0CoM8zCm+/3fdMgoaLRpeZWe/AfDK6b4qOjk/sez/J0JUFCGr+JbMwjsduoazsuQowu9L9DLP9Q5UkJje4BD7MHznaeu9/XfVng/MvyaEWArA/VUJeKQesHe76tR511/+n3+bdzlIh8Zw/3wfFxmg1OTNA99/vLkXrQzHDTuV/yj1pxykL4xFtN0OIssW1IKncJeKtkO/OHGT55ypz52Daj6bNKqvxiTuzeEhv5M+5ppyIPcRf1uj/7IaPKttCgZAntEqBTIR9MbyXFeAZVayzaFnLl2okeam5XreeZbj+Y1h2ZjxiIuWoab3MLndSekVfLtfa63gtcWIf8CIvZO2wJoH8v73y0U78JsfWVaTM09ZCfFlHHA/bWqZ6laAjW+mWLO/c77DcYkB3IBzaMVNfc6mfTcGFIC+biWeYpKgA0zC6rByUPbmbIoMueP9zqJwqUaM90Nwd6559inBB107/BK3Ktb3b+37mMCstetIPB9e4EFpGMjhmnL/G81jS53ACWLXJYzt7mKU/fEsiW93MtaB+Le46OEC18y/4G8F7p/nnH7i0kO74ukxbnc4PlpiM7iWT6ra2Cyy+nzEgdXCNXywIxr05TbCQDwX6/NY8k7Hokgdfyz+1Pq3sX0yCcWRPaoB26YF12KYFQ== kieran.meinhardt@gmail.com";
    rilke = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDWRcTILWzSzOAWqwBjJC97K9wKm+pfxk15L5AiTUlUMyBzVQsU+d2jUEe1rmp+kjVXL0qgNMI+eBzEAzaafUMdCPeIkMyeiOlXaYxq8NHApcZUSYV9CmzWqePj8Dlu8uexoF3TJi6tj3mmGM8BY50qkwk4mlT/5xLPPnh/HHyFmoZlj6183Z4vJRnK8UZ6x2gevVaE36lP27MikZ/MQ6/PaHJ5TNZy63rQwzKXw6ZQMx4JU22CwyZqPn1wjlGEkpJFOBKtnypURdVBsPydaZd/I7b/13FMwso2hSUoXqeV6iaeno2FWOrB3cAaFogNWKPRkEacKEE5mQOvLGGdG1Xp u0_a138@localhost";
    wilde = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDyTnGhFq0Q+vghNhrqNrAyY+CsN7nNz8bPfiwIwNpjk kfm@wilde";
  };
  unstable = import <nixos-unstable> {};
  wtf-config = pkgs.writeText "config.yml" (builtins.toJSON {
    wtf = {
      colors = {
        background = "black";
        border = {
          focusable = "darkslateblue";
          focused = "orange";
          normal = "gray";
        };
      };
      grid = {
        columns = [40 54];
        rows = [10 20];
      };
      refreshInterval = 1;
      mods = {
        clocks = {
          colors.rows = {
            even = "lightblue";
            odd = "white";
          };
          enabled = true;
          locations = {
            "Berlin" = "Europe/Berlin";
            "London" = "Europe/London";
            "Moscow" = "Europe/Moscow";
            "New York" = "America/New York";
          };
          position = {
            top = 0;
            left = 0;
            height = 1;
            width = 1;
          };
          refreshInterval = 15;
          sort = "alphabetical";
        };
        security = {
          enabled = true;
          position = {
            top = 1;
            left = 0;
            height = 1;
            width = 1;
          };
          refreshInterval = 300;
        };
        prettyweather = {
          enabled = true;
          city = "Berlin";
          position = {
            top = 0;
            left = 1;
            height = 1;
            width = 1;
          };
          refreshInterval = 300;
        };
        cmdrunner = {
          enabled = true;
          args = [ "status" ];
          cmd = "traadfri";
          position = {
            top = 1;
            left = 1;
            height = 1;
            width = 1;
          };
          refreshInterval = 2;
        };
      };
    };
  });
in {
  imports = [
    ./hardware-configuration.nix
    ./containers.nix
    <niveum/configs/distrobump.nix>
    <niveum/configs/nixpkgs-unstable.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/tmux.nix>
    <niveum/configs/version.nix>
    <niveum/configs/traadfri.nix>
    <niveum/modules/retiolum.nix>
    <niveum/modules/telegram-bot.nix>
    {
      nixpkgs = {
        config.packageOverrides = pkgs: {
          writeDash = pkgs.writers.writeDash;
          writeDashBin = pkgs.writers.writeDashBin;
        };
        overlays = [
          (import <niveum/overlays/toml.nix>)
        ];
      };

      services.matterbridge = {
        enable = true;
        configPath = toString (pkgs.writeTOML (import <niveum/dot/matterbridge.nix> {
          token = lib.strings.fileContents <secrets/telegram/kmein.token>;
        }));
      };
    }
    {
      services.keybase.enable = true;
    }
    {
      environment.systemPackages = [ pkgs.wtf ];

      systemd.services.wtf = {
        wantedBy = [ "multi-user.target" ];
        script = "${pkgs.tmux}/bin/tmux new-session -d '${pkgs.wtf}/bin/wtf --config=${wtf-config}'";
        serviceConfig.Type = "oneshot";
      };
    }
    /*
    {
      imports = [ <stockholm/krebs/3modules/urlwatch.nix> ];

      krebs.urlwatch = {
        enable = true;
        onCalendar = "*-*-* 05:00:00";
        sendmail.enable = false;
        telegram = {
          enable = true;
          chatId = [ "18980945" ];
          botToken = lib.strings.fileContents <secrets/telegram/kmein.token>;
        };
        urls = [];
      };
    }
    */
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
  environment.variables.HTOPRC = toString <niveum/dot/htoprc>;

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
    extraGroups = [ "wheel" ];
    hashedPassword = "$6$w9hXyGFl/.IZBXk$5OiWzS1G.5hImhh1YQmZiCXYNAJhi3X6Y3uSLupJNYYXPLMsQpx2fwF4Xr2uYzGMV8Foqh8TgUavx1APD9rcb/";
    shell = pkgs.bash;
  };

  security.sudo.enable = true;

  services.openssh = {
    enable = true;
    ports = [ 22022 ];
    passwordAuthentication = false;
  };

  users.users.root.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
    sshKey.wilde
  ];

  users.users.me.openssh.authorizedKeys.keys = [
    sshKey.homeros
    sshKey.scardanelli
    sshKey.rilke
    sshKey.wilde
  ];

  networking.retiolum = {
    ipv4 = "10.243.2.3";
    ipv6 = "42:0:3c46:3ec0:7aad:d1d5:9842:da4c";
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <secrets/retiolum.key>;
    mode = "400";
  };

  system.stateVersion = "18.09";
}
