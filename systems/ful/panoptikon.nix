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
        reporters = [irc-kmein];
      };
      btc = {
        script = panoptikon.url "https://eur.rate.sx/1BTC";
        reporters = [telegram-kmein];
        loadCredential = [
          "token:${config.age.secrets.telegram-token-kmein.path}"
        ];
      };
      kratylos = {
        script = panoptikon.url "https://kratylos.reichert-online.org/current_issue/KRATYLOS";
        reporters = [irc-kmein];
      };
      zeno-free = {
        script = panoptikon.urlSelector ".zenoCOMain" "http://www.zeno.org/Lesesaal/M/E-Books";
        reporters = [irc-kmein];
      };
      carolinawelslau = {
        script = panoptikon.urlSelector "#main" "https://carolinawelslau.de/";
        reporters = [irc-kmein];
      };
      humboldt-preis = {
        script = panoptikon.urlSelector "#content-core" "https://www.hu-berlin.de/de/ueberblick/menschen/ehrungen/humboldtpreis";
        reporters = [irc-kmein];
      };
      lisalittmann = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/";
        reporters = [irc-kmein];
      };
      lisalittmann-archive = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/archive/";
        reporters = [irc-kmein];
      };
      lisalittmann-projects = {
        script = panoptikon.urlSelector "#site-content" "https://lisalittmann.de/projects/";
        reporters = [irc-kmein];
      };
      tatort = {
        script = panoptikon.urlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
        reporters = [irc-kmein];
      };
      warpgrid-idiomarium = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/idiomarium/";
        reporters = [irc-kmein];
      };
      warpgrid-futurism = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/futurism/";
        reporters = [irc-kmein];
      };
      warpgrid-imagiary = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/imagiary/";
        reporters = [irc-kmein];
      };
      warpgrid-alchemy = {
        script = panoptikon.urlSelector "#site-content" "https://warpgrid.de/alchemy/";
        reporters = [irc-kmein];
      };
      indogermanische-forschungen = {
        script = panoptikon.urlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
        reporters = [irc-kmein];
      };
      ig-neuigkeiten = {
        script = panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
        reporters = [irc-kmein];
      };
      ig-tagungen = {
        script = panoptikon.urlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
        reporters = [irc-kmein];
      };
      fu-distant = {
        script = panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/en/e/ma-distant/Termine/index.html";
        reporters = [irc-kmein];
      };
      fu-aegyptologie = {
        script = panoptikon.urlSelector "#current_events" "https://www.geschkult.fu-berlin.de/e/aegyptologie/termine/index.html";
        reporters = [irc-kmein];
      };
    };
  };
}
