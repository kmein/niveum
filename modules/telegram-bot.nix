{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.niveum.bots;

  botService = name: bot:
    nameValuePair "bot-${name}" {
      enable = bot.enable;
      startAt = bot.time;
      serviceConfig = {
        Type = "oneshot";
        LoadCredential = lib.optionals (bot.telegram.enable) [
          "telegram-token:${bot.telegram.tokenFile}"
        ] ++ lib.optionals (bot.mastodon.enable) [
          "mastodon-token:${bot.mastodon.tokenFile}"
        ] ++ lib.optionals (bot.matrix.enable) [
          "matrix-token:${bot.matrix.tokenFile}"
        ];
      };
      wants = ["network-online.target"];
      script = ''
        QUOTE=$(${bot.command})
        if [ -n "$QUOTE" ]; then
          echo $QUOTE >&2

          ${lib.optionalString (bot.matrix.enable) ''
            export MATRIX_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/matrix-token")"
            export JSON_PAYLOAD=$(${pkgs.jq}/bin/jq -n --arg msgtype "m.text" --arg body "$QUOTE" '{msgtype: $msgtype, body: $body}')
            ${strings.concatStringsSep "\n" (map (chatId: ''
              ${pkgs.curl}/bin/curl -X POST "https://${bot.matrix.homeserver}/_matrix/client/r0/rooms/${chatId}/send/m.room.message" \
                -d "$JSON_PAYLOAD" \
                -H "Authorization: Bearer $MATRIX_TOKEN" \
                -H "Content-Type: application/json"
            '') bot.matrix.chatIds)}
          ''}

          ${lib.optionalString (bot.mastodon.enable) ''
            export MASTODON_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/mastodon-token")"
            ${pkgs.curl}/bin/curl -X POST "https://${bot.mastodon.homeserver}/api/v1/statuses" \
              -H "Authorization: Bearer $MASTODON_TOKEN" \
              -d status="$QUOTE" \
              -d "language=${bot.mastodon.language}" \
              -d "visibility=public"
          ''}

          ${lib.optionalString (bot.telegram.enable) ''
            export TELEGRAM_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/telegram-token")"
            ${strings.concatStringsSep "\n" (map (chatId: ''
              ${pkgs.curl}/bin/curl -X POST "https://api.telegram.org/bot''${TELEGRAM_TOKEN}/sendMessage" \
                -d chat_id="${chatId}" \
                -d text="$QUOTE" ${
                lib.strings.optionalString (bot.telegram.parseMode != null)
                "-d parse_mode=${bot.telegram.parseMode}"
              } | ${pkgs.jq}/bin/jq -e .ok
            '')
            bot.telegram.chatIds)}
          ''}
        fi
      '';
    };
in {
  options.niveum.bots = mkOption {
    type = types.attrsOf (types.submodule {
      options = {
        enable = mkEnableOption "Mastodon and Telegram bot";
        time = mkOption {type = types.str;};
        command = mkOption {type = types.str;};
        matrix = mkOption {
          default = {};
          type = types.submodule {
            options = {
              enable = mkEnableOption "Posting to Matrix";
              tokenFile = mkOption {type = types.path;};
              homeserver = mkOption {
                type = types.str;
              };
              chatIds = mkOption {
                type = types.listOf types.str;
              };
            };
          };
        };
        mastodon = mkOption {
          default = {};
          type = types.submodule {
            options = {
              enable = mkEnableOption "Posting to Mastodon";
              language = mkOption {
                type = types.str;
                default = "en";
              };
              tokenFile = mkOption {type = types.path;};
              homeserver = mkOption {
                type = types.str;
                default = "social.krebsco.de";
              };
            };
          };
        };
        telegram = mkOption {
          default = {};
          type = types.submodule {
            options = {
              enable = mkEnableOption "Posting to Telegram";
              tokenFile = mkOption {type = types.path;};
              chatIds = mkOption {
                type = types.listOf (types.strMatching "-?[0-9]+|@[A-Za-z0-9]+");
              };
              parseMode = mkOption {
                type = types.nullOr (types.enum ["HTML" "Markdown"]);
                default = null;
              };
            };
          };
        };
      };
    });
    default = {};
  };

  config = {systemd.services = attrsets.mapAttrs' botService cfg;};
}
