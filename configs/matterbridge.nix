{ pkgs, lib, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      matterbridge = (import (super.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "e45d91ee65db293a172ec506759d1248e40c35f5";
        sha256 = "03cjs5xcx09lw0djyrx2kfakw7gkg4iqmy9w25azai62im39l30k";
      }) {}).matterbridge;
    })
  ];

  services.matterbridge = {
    enable = true;
    configPath =
    let bridgeBotToken = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    in toString ((pkgs.formats.toml {}).generate "config.toml" {
      general = {
        RemoteNickFormat = "[{NICK}] ";
        Charset = "utf-8";
      };
      telegram.kmein.Token = bridgeBotToken;
      irc =
      let Nick = "ponte";
      in {
        hackint = { Server = "irc.hackint.org:6697"; UseTLS = true; inherit Nick; };
      };
      mumble.lassulus = {
        Server = "lassul.us:64738";
        Nick = "krebs_bridge";
        SkipTLSVerify = true;
      };
      gateway = [
        {
          name = "krebs-bridge";
          enable = true;
          inout = [
            {
              account = "irc.hackint";
              channel = "#krebs";
            }
            {
              account = "telegram.kmein";
              channel = "-330372458";
            }
            {
              account = "mumble.lassulus";
              channel = 6; # "nixos"
            }
          ];
        }
      ];
    });
  };
}
