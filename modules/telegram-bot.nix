{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.niveum.telegramBots;

  botService = name: bot:
    nameValuePair "telegram-bot-${name}" {
      enable = bot.enable;
      startAt = bot.time;
      serviceConfig.Type = "oneshot";
      wants = [ "network-online.target" ];
      script = strings.concatStringsSep "\n" ([ "QUOTE=$(${bot.command})" "if [ -n \"$QUOTE\" ]; then" ]
        ++ map (chatId: ''
          ${pkgs.curl}/bin/curl -s -X POST "https://api.telegram.org/bot${bot.token}/sendMessage" \
            -d chat_id="${chatId}" \
            -d text="$QUOTE" ${
              lib.strings.optionalString (bot.parseMode != null)
              "-d parse_mode=${bot.parseMode}"
            }
        '') bot.chatIds
        ++ [ "fi" ]);
    };
in {
  options.niveum.telegramBots = mkOption {
    type = types.attrsOf (types.submodule {
      options = {
        enable = mkEnableOption "Telegram bot";
        time = mkOption { type = types.str; };
        token = mkOption { type = types.strMatching "[0-9A-Za-z:-]+"; };
        chatIds = mkOption {
          type = types.listOf (types.strMatching "-?[0-9]+|@[A-Za-z0-9]+");
        };
        command = mkOption { type = types.str; };
        parseMode = mkOption {
          type = types.nullOr (types.enum [ "HTML" "Markdown" ]);
          default = null;
        };
      };
    });
    default = { };
  };

  config = { systemd.services = attrsets.mapAttrs' botService cfg; };
}
