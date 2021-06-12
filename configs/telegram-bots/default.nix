{ config, pkgs, lib, ... }:
let
  telebots = let
    telebots-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "telebots";
      rev = "ce613f5b298721c6eaa7af25a666bd65388a0daf";
      sha256 = "0bl02976kxjbbbmvcnjjgpmxrmpwplw9qcldxk5h3wngzixxizhg";
    };
  in pkgs.callPackage telebots-package {};
in {
  imports = [
    ./literature-quote.nix
    ./autorenkalender.nix
    ./proverb.nix
    ./nachtischsatan.nix
    ./tlg-wotd.nix
    <niveum/modules/telegram-bot.nix>
  ];

  systemd.services.telegram-reverse = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram reverse bot";
    path = [ pkgs.ffmpeg ];
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/reverse.token>;
    enable = true;
    script = "${telebots}/bin/telegram-reverse";
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-betacode = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram beta code bot";
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/betacode.token>;
    enable = true;
    script = "${telebots}/bin/telegram-betacode";
    serviceConfig.Restart = "always";
  };
}
