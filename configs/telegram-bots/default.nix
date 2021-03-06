{ config, pkgs, lib, ... }:
let
  telebots = let
    telebots-package = pkgs.fetchFromGitHub {
      owner = "kmein";
      repo = "telebots";
      rev = "22931c9457e092c4e413555dbe61819d77844246";
      sha256 = "0byp3w6li3fin7ry0ki4rmgkaajdil424y5pc1j7ci9mpws0s5ik";
    };
  in pkgs.callPackage telebots-package {};
  reverseDirectory = "/run/telegram-reverse";
  proverbDirectory = "/run/telegram-proverb";
  inherit (import <niveum/lib>) tmpfilesConfig;
in {
  imports = [
    ./literature-quote.nix
    ./autorenkalender.nix
    ./nachtischsatan.nix
    ./tlg-wotd.nix
    <niveum/modules/telegram-bot.nix>
  ];

  systemd.tmpfiles.rules = map (path: tmpfilesConfig {
    type = "d";
    mode = "0750";
    age = "1h";
    inherit path;
  }) [ reverseDirectory proverbDirectory ];

  systemd.services.telegram-reverse = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram reverse bot";
    path = [ pkgs.ffmpeg ];
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/reverse.token>;
    enable = true;
    script = "${telebots}/bin/telegram-reverse";
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = reverseDirectory;
  };

  systemd.services.telegram-betacode = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram beta code bot";
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/betacode.token>;
    enable = true;
    script = "${telebots}/bin/telegram-betacode";
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-proverb = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram proverb bot";
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/proverb.token>;
    enable = true;
    script = "${telebots}/bin/telegram-proverb";
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = proverbDirectory;
  };
}
