{
  config,
  pkgs,
  lib,
  niveumLib,
  niveumPackages,
  ...
}: let
  panoptikon = niveumLib.panoptikon {inherit pkgs lib niveumPackages config;};

  irc-xxx = panoptikon.kpaste-irc {
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

  irc-kmein = panoptikon.kpaste-irc {
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
        script = panoptikon.urlJSON {
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
        script = panoptikon.url "http://lammla.info/index.php?reihe=30";
        reporters = [irc-kmein matrix-kmein];
      };
      kratylos = {
        script = panoptikon.url "https://kratylos.reichert-online.org/current_issue/KRATYLOS";
        reporters = [irc-kmein matrix-kmein];
      };
      kobudo-tesshinkan = {
        script = panoptikon.url "https://kobudo-tesshinkan.eu/index.php/de/termine-berichte/lehrgaenge/";
        reporters = [irc-kmein telegram-kmein matrix-kmein];
      };
      zeno-free = {
        script = panoptikon.urlSelector ".zenoCOMain" "http://www.zeno.org/Lesesaal/M/E-Books";
        reporters = [irc-kmein matrix-kmein];
      };
      carolinawelslau = {
        script = panoptikon.urlSelector "#main" "https://carolinawelslau.de/";
        reporters = [irc-kmein matrix-kmein];
      };
      humboldt-preis = {
        script = panoptikon.urlSelector "#content-core" "https://www.hu-berlin.de/de/ueberblick/menschen/ehrungen/humboldtpreis";
        reporters = [irc-kmein matrix-kmein];
      };
      lisalittmann = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/";
        reporters = [irc-kmein matrix-kmein];
      };
      lisalittmann-archive = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/archive/";
        reporters = [irc-kmein matrix-kmein];
      };
      lisalittmann-projects = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/projects/";
        reporters = [irc-kmein matrix-kmein];
      };
      tatort = {
        script = panoptikon.urlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
        reporters = [irc-kmein matrix-kmein];
      };
      warpgrid-idiomarium = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/idiomarium/";
        reporters = [irc-kmein matrix-kmein];
      };
      warpgrid-futurism = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/futurism/";
        reporters = [irc-kmein matrix-kmein];
      };
      warpgrid-imagiary = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/imagiary/";
        reporters = [irc-kmein matrix-kmein];
      };
      warpgrid-alchemy = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/alchemy/";
        reporters = [irc-kmein matrix-kmein];
      };
      indogermanische-forschungen = {
        script = panoptikon.urlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
        reporters = [irc-kmein matrix-kmein];
      };
      ig-neuigkeiten = {
        script = panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
        reporters = [irc-kmein matrix-kmein];
      };
      ig-tagungen = {
        script = panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
        reporters = [irc-kmein matrix-kmein];
      };
      fu-distant = {
        script = panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/en/e/ma-distant/Termine/index.html";
        reporters = [irc-kmein matrix-kmein];
      };
      fu-aegyptologie = {
        script = panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/e/aegyptologie/termine/index.html";
        reporters = [irc-kmein matrix-kmein];
      };
    };
  };
}
