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
    target = "#xxx";
    retiolumLink = true;
  };

  irc-kmein = panoptikon.kpaste-irc {
    messagePrefix = "$PANOPTIKON_WATCHER: ";
    target = "kmein";
    nick = "panoptikon-kmein";
    retiolumLink = false;
  };
in {
  services.panoptikon = {
    enable = true;
    watchers = {
      "github-meta" = {
        script = panoptikon.urlJSON {} "https://api.github.com/meta";
        reporters = [irc-xxx];
      };
      lammla = {
        script = panoptikon.url "http://lammla.info/index.php?reihe=30";
        reporters = [irc-kmein];
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
      fxght-or-flxght = {
        script = panoptikon.urlJSON {
          jqScript = ''
            .answers | map(
              select(.type == "answer")
              | {
                 question: .tell,
                 answer: .answer,
                 date: .createdAt,
                 media: .media | map(.url)
              }
            )
          '';
        } "https://api.tellonym.me/profiles/name/fxght.or.flxght?limit=20";
        reporters = [irc-kmein];
      };
    };
  };
}
