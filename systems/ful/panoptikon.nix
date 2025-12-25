{
  config,
  pkgs,
  lib,
  ...
}: let
  irc-xxx = lib.panoptikon.kpaste-irc {
    target = lib.escapeShellArg "#xxx";
    retiolumLink = true;
  };

  matrix = {
    server ? "matrix.4d2.org",
    target,
  }:
    pkgs.writers.writeDash "matrix-reporter" ''
      export RAW_MESSAGE="$(cat)"
      export MESSAGE=$(printf '<b>%s</b><br><pre>%s</pre>' "$PANOPTIKON_WATCHER" "$RAW_MESSAGE")
      export MATRIX_TOKEN="$(cat ${config.age.secrets.matrix-token-lakai.path})"
      export JSON_PAYLOAD=$(${pkgs.jq}/bin/jq -n --arg msgtype "m.text" --arg body "$RAW_MESSAGE" --arg formattedBody "$MESSAGE" '{msgtype: $msgtype, body: $body, format: "org.matrix.custom.html", formatted_body: $formattedBody}')
      ${pkgs.curl}/bin/curl -X POST "https://${server}/_matrix/client/r0/rooms/${target}/send/m.room.message" \
        -d "$JSON_PAYLOAD" \
        -H "Authorization: Bearer $MATRIX_TOKEN" \
        -H "Content-Type: application/json"
    '';

  matrix-kmein = matrix { target = "!zlwCuPiCNMSxDviFzA:4d2.org"; };

  telegram-kmein = let
    chatId = "-1001796440545";
  in
    pkgs.writers.writeDash "telegram-fulltext" ''
      export TOKEN="$(cat "$CREDENTIALS_DIRECTORY/token")"
      ${pkgs.curl}/bin/curl -X POST "https://api.telegram.org/bot''${TOKEN}/sendMessage" \
        -d chat_id=${chatId} \
        -d text="$(cat)" \
        | ${pkgs.jq}/bin/jq -e .ok
    '';

  irc-kmein = lib.panoptikon.kpaste-irc {
    messagePrefix = "$PANOPTIKON_WATCHER: ";
    target = "kmein";
    nick = "panoptikon-kmein";
    retiolumLink = false;
  };
in {
  age.secrets.telegram-token-kmein.file = ../../secrets/telegram-token-kmein.age;
  age.secrets.matrix-token-lakai = {
    file = ../../secrets/matrix-token-lakai.age;
    owner = "panoptikon";
    group = "panoptikon";
    mode = "400";
  };

  services.panoptikon = {
    enable = true;
    watchers = {
      "github-meta" = {
        script = lib.panoptikon.urlJSON {
          jqScript = ''
            {
              ssh_key_fingerprints: .ssh_key_fingerprints,
              ssh_keys: .ssh_keys
            }
          '';
        } "https://api.github.com/meta";
        reporters = [irc-xxx];
      };
      lammla = {
        script = lib.panoptikon.url "http://lammla.info/index.php?reihe=30";
        reporters = [matrix-kmein];
      };
      kratylos = {
        script = lib.panoptikon.url "https://kratylos.reichert-online.org/current_issue/KRATYLOS";
        reporters = [matrix-kmein];
      };
      kobudo-tesshinkan = {
        script = lib.panoptikon.url "https://kobudo-tesshinkan.eu/index.php/de/termine-berichte/lehrgaenge/";
        reporters = [telegram-kmein matrix-kmein];
      };
      zeno-free = {
        script = lib.panoptikon.urlSelector ".zenoCOMain" "http://www.zeno.org/Lesesaal/M/E-Books";
        reporters = [matrix-kmein];
      };
      carolinawelslau = {
        script = lib.panoptikon.urlSelector "#main" "https://carolinawelslau.de/";
        reporters = [matrix-kmein];
      };
      humboldt-preis = {
        script = lib.panoptikon.urlSelector "#content-core" "https://www.hu-berlin.de/de/ueberblick/menschen/ehrungen/humboldtpreis";
        reporters = [matrix-kmein];
      };
      lisalittmann = {
        script = lib.panoptikon.urlSelector "#site-content" "https://lisalittmann.de/";
        reporters = [matrix-kmein];
      };
      lisalittmann-archive = {
        script = lib.panoptikon.urlSelector "#site-content" "https://lisalittmann.de/archive/";
        reporters = [matrix-kmein];
      };
      lisalittmann-projects = {
        script = lib.panoptikon.urlSelector "#site-content" "https://lisalittmann.de/projects/";
        reporters = [matrix-kmein];
      };
      tatort = {
        script = lib.panoptikon.urlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
        reporters = [matrix-kmein];
      };
      warpgrid-idiomarium = {
        script = lib.panoptikon.urlSelector "#site-content" "https://warpgrid.de/idiomarium/";
        reporters = [matrix-kmein];
      };
      warpgrid-futurism = {
        script = lib.panoptikon.urlSelector "#site-content" "https://warpgrid.de/futurism/";
        reporters = [matrix-kmein];
      };
      warpgrid-imagiary = {
        script = lib.panoptikon.urlSelector "#site-content" "https://warpgrid.de/imagiary/";
        reporters = [matrix-kmein];
      };
      warpgrid-alchemy = {
        script = lib.panoptikon.urlSelector "#site-content" "https://warpgrid.de/alchemy/";
        reporters = [matrix-kmein];
      };
      indogermanische-forschungen = {
        script = lib.panoptikon.urlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
        reporters = [matrix-kmein];
      };
      ig-neuigkeiten = {
        script = lib.panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
        reporters = [matrix-kmein];
      };
      ig-tagungen = {
        script = lib.panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
        reporters = [matrix-kmein];
      };
      fu-distant = {
        script = lib.panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/en/e/ma-distant/Termine/index.html";
        reporters = [matrix-kmein];
      };
      fu-aegyptologie = {
        script = lib.panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/e/aegyptologie/termine/index.html";
        reporters = [matrix-kmein];
      };
    };
  };
}
