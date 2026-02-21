{
  config,
  pkgs,
  lib,
  ...
}:
let
  irc-xxx = pkgs.panoptikonReporters.kpaste-irc {
    target = lib.escapeShellArg "#xxx";
    retiolumLink = true;
  };

  matrix-kmein = pkgs.panoptikonReporters.matrix {
    homeserver = "matrix.4d2.org";
    roomId = lib.escapeShellArg "!zlwCuPiCNMSxDviFzA:4d2.org";
    tokenPath = config.age.secrets.matrix-token-lakai.path;
  };

  telegram-kmein = pkgs.panoptikonReporters.telegram {
    tokenPath = config.age.secrets.telegram-token-kmein.path;
    chatId = "-1001796440545";
  };

  irc-kmein = pkgs.panoptikonReporters.kpaste-irc {
    messagePrefix = "$PANOPTIKON_WATCHER: ";
    target = "kmein";
    nick = "panoptikon-kmein";
    retiolumLink = false;
  };
in
{
  age.secrets.telegram-token-kmein = {
    file = ../../secrets/telegram-token-kmein.age;
    owner = "panoptikon";
    group = "panoptikon";
    mode = "400";
  };
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
        script = pkgs.panoptikonWatchers.json {
          jqScript = ''
            {
              ssh_key_fingerprints: .ssh_key_fingerprints,
              ssh_keys: .ssh_keys
            }
          '';
        } "https://api.github.com/meta";
        reporters = [ irc-xxx ];
      };
      kobudo-tesshinkan = {
        script = pkgs.panoptikonWatchers.html "https://kobudo-tesshinkan.eu/index.php/de/termine-berichte/lehrgaenge/";
        reporters = [
          telegram-kmein
          matrix-kmein
        ];
      };
      carolinawelslau = {
        script = pkgs.panoptikonWatchers.htmlSelector "#main" "https://carolinawelslau.de/";
        reporters = [ matrix-kmein ];
      };
      humboldt-preis = {
        script = pkgs.panoptikonWatchers.htmlSelector "#content-core" "https://www.hu-berlin.de/de/ueberblick/menschen/ehrungen/humboldtpreis";
        reporters = [ matrix-kmein ];
      };
      lisalittmann = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/";
        reporters = [ matrix-kmein ];
      };
      lisalittmann-archive = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/archive/";
        reporters = [ matrix-kmein ];
      };
      lisalittmann-projects = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://lisalittmann.de/projects/";
        reporters = [ matrix-kmein ];
      };
      tatort = {
        script = pkgs.panoptikonWatchers.htmlSelector ".linklist" "https://www.daserste.de/unterhaltung/krimi/tatort/sendung/index.html";
        reporters = [ matrix-kmein ];
      };
      warpgrid-idiomarium = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/idiomarium/";
        reporters = [ matrix-kmein ];
      };
      warpgrid-futurism = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/futurism/";
        reporters = [ matrix-kmein ];
      };
      warpgrid-imagiary = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/imagiary/";
        reporters = [ matrix-kmein ];
      };
      warpgrid-alchemy = {
        script = pkgs.panoptikonWatchers.htmlSelector "#site-content" "https://warpgrid.de/alchemy/";
        reporters = [ matrix-kmein ];
      };
      indogermanische-forschungen = {
        script = pkgs.panoptikonWatchers.htmlSelector "#latestIssue" "https://www.degruyter.com/journal/key/INDO/html";
        reporters = [ matrix-kmein ];
      };
      ig-neuigkeiten = {
        script = pkgs.panoptikonWatchers.htmlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/aktuelles/neuigkeiten.html";
        reporters = [ matrix-kmein ];
      };
      ig-tagungen = {
        script = pkgs.panoptikonWatchers.htmlSelector "[itemprop=articleBody]" "https://www.indogermanistik.org/tagungen/tagungen-der-ig.html";
        reporters = [ matrix-kmein ];
      };
      fu-distant = {
        script = pkgs.panoptikonWatchers.htmlSelector "#current_events" "https://www.geschkult.fu-berlin.de/en/e/ma-distant/Termine/index.html";
        reporters = [ matrix-kmein ];
      };
      fu-aegyptologie = {
        script = pkgs.panoptikonWatchers.htmlSelector "#current_events" "https://www.geschkult.fu-berlin.de/e/aegyptologie/termine/index.html";
        reporters = [ matrix-kmein ];
      };
    };
  };
}
