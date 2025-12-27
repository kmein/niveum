{
  pkgs,
  lib,
  ...
}:
{
  services.matterbridge = {
    enable = false;
    configPath =
      let
        bridgeBotToken = lib.strings.fileContents <system-secrets/telegram/krebs.token>;
      in
      toString (
        (pkgs.formats.toml { }).generate "config.toml" {
          general = {
            RemoteNickFormat = "[{NICK}] ";
            Charset = "utf-8";
          };
          telegram.krebs.Token = bridgeBotToken;
          irc =
            let
              Nick = "ponte";
            in
            {
              hackint = {
                Server = "irc.hackint.org:6697";
                UseTLS = true;
                inherit Nick;
              };
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
                  account = "telegram.krebs";
                  channel = "-330372458";
                }
                {
                  account = "mumble.lassulus";
                  channel = 6; # "nixos"
                }
              ];
            }
          ];
        }
      );
  };
}
