{
  lib,
  pkgs,
  config,
  niveumPackages,
  ...
}: let
  inherit (import ../../lib) tmpfilesConfig;
  liquidsoapDirectory = "/var/cache/liquidsoap";
  icecastPassword = "hackme";
  refresh-qasaid = pkgs.writers.writeDashBin "refresh-qasaid" ''
    (
      for i in $(seq 1 22)
      do
        ${pkgs.curl}/bin/curl -sSL "https://www.hindawi.org/poems/$i/"
      done
    ) | ${pkgs.htmlq}/bin/htmlq '.poems li' \
      | ${pkgs.fq}/bin/fq -d html  '
        .html.body.li
        | map(.a
          | {
            id: .[0].["@href"] | sub("/poems/"; "") | sub("/$"; "") | tonumber,
            poem: .[0].["#text"],
            author: .[1].["#text"]
          })
      ' | ${niveumPackages.cyberlocker-tools}/bin/cput qasaid.json
  '';
  qasida-poem = pkgs.writers.writeDash "qasida.sh" ''
    set -efu
    ${pkgs.jq}/bin/jq -c '.[]' < ${pkgs.fetchurl {
      url = "https://c.krebsco.de/qasaid.json";
      sha256 = "0vh1jzdrvjrdyq7dzya9k9g3jyli9jr0zfsqb2m1phm39psy4g2b";
    }} \
      | shuf -n1 \
      | ${pkgs.jq}/bin/jq -r '"annotate:title=\"\(.poem) | https://www.hindawi.org/poems/\(.id)/\",artist=\"\(.author)\":https://downloads.hindawi.org/poems/\(.id)/\(.id).m4a"'
  '';
  lyrikline-poem = pkgs.writers.writeDash "lyrikline.sh" ''
    set -efu

    html=$(mktemp)
    trap clean EXIT
    clean() {
      rm "$html"
    }

    lyrikline=https://www.lyrikline.org
    random_route="$(${pkgs.curl}/bin/curl -sSL "$lyrikline/index.php/tools/getrandompoem" --data-raw 'lang=de' --compressed | ${pkgs.jq}/bin/jq -r .link)"
    poem_url="$lyrikline$random_route"

    ${pkgs.curl}/bin/curl -sSL "$poem_url" > "$html"

    poem_file="$(${pkgs.gnugrep}/bin/grep -o 'https://.*\.mp3' "$html" | head -n1)"

    author="$(${pkgs.htmlq}/bin/htmlq -f "$html" --text '#gedicht-autor')"
    title="$(${pkgs.htmlq}/bin/htmlq -f "$html" --text .gedicht-originaltitel)"

    echo "annotate:title=\"$title | $poem_url\",artist=\"$author\":$poem_file"
  '';
  stavenhagen-poem = pkgs.writers.writeDash "stavenhagen.sh" ''
    base=https://www.deutschelyrik.de
    author=$(${pkgs.curl}/bin/curl -sSL "$base" | ${pkgs.htmlq}/bin/htmlq option --attribute value | shuf -n1)
    poem=$(${pkgs.curl}/bin/curl -sSL "$base/$author" | ${pkgs.htmlq}/bin/htmlq '#mnav2 li > a' --attribute href | shuf -n1)

    html=$(mktemp)
    trap clean EXIT
    clean() {
      rm "$html"
    }

    ${pkgs.curl}/bin/curl -sSL "$base/$poem" > "$html"

    printf "annotate:title=\"%s | %s\",artist=\"%s\":$base/%s\n" \
      "$(${pkgs.htmlq}/bin/htmlq --text '.ce_text h1' -f "$html")" \
      "$base/$poem" \
      "$(${pkgs.htmlq}/bin/htmlq --text 'h1 + p em' -f "$html")" \
      "$(${pkgs.htmlq}/bin/htmlq 'audio source' --attribute src -f "$html")"
  '';
  wikipedia-article = pkgs.writers.writeDash "wikipedia.sh" ''
    set -efu
    opus=$(mktemp ${liquidsoapDirectory}/wikipedia.XXX.opus)

    html=$(mktemp)
    trap clean EXIT
    clean() {
      rm "$html"
    }

    ${pkgs.curl}/bin/curl -sSL https://de.wikipedia.org/wiki/Spezial:Zuf%C3%A4llige_Seite > "$html"

    ${pkgs.htmlq}/bin/htmlq '.mw-parser-output p' --text -f "$html" \
      | ${pkgs.gnused}/bin/sed 's/\[[0-9]\+]//g' \
      | ${pkgs.espeak}/bin/espeak -v german-mbrola-6 -w /dev/stdout \
      | ${pkgs.opusTools}/bin/opusenc --quiet - "$opus"

    printf "annotate:title=\"%s\":%s" \
      "$(${pkgs.htmlq}/bin/htmlq -f "$html" --text h1)" \
      "$opus"
  '';
in {
  # https://github.com/savonet/liquidsoap/issues/1043#issuecomment-593354427
  services.liquidsoap.streams.radio = pkgs.writeText "lyrikline.liq" ''
    set("protocol.external.curl","${pkgs.torsocks}/bin/torsocks ${pkgs.curl}/bin/curl")

    def random_url(script) =
      mksafe(audio_to_stereo(request.dynamic.list(
        fun () -> list.map(request.create, process.read.lines(script))
      )))
    end

    def make_streams(name, audio, ~description, ~genre) =
      output.icecast(%vorbis, audio, mount = name ^ ".ogg", genre = genre, description = description,
        port = ${toString config.services.icecast.listen.port},
        password = "${icecastPassword}",
      )
      output.icecast(%opus, audio, mount = name ^ ".opus", genre = genre, description = description,
        port = ${toString config.services.icecast.listen.port},
        password = "${icecastPassword}",
      )
    end

    # make_streams("lyrikline", random_url("${lyrikline-poem}"), description="lyrikline. listen to the poet (unofficial)", genre="poetry")
    make_streams("qasida", random_url("${qasida-poem}"), description="Qasa'id. Classical arabic poetry", genre="poetry")
    make_streams("lyrik", random_url("${stavenhagen-poem}"), description="Fritz Stavenhagen – Lyrik für alle | www.deutschelyrik.de", genre="poetry")
    # make_streams("wikipedia", random_url("${wikipedia-article}"), description="Zufällige Artikel von Wikipedia", genre="useless knowledge")
  '';

  systemd.services.radio = {
    environment.TMPDIR = liquidsoapDirectory;
    wants = ["network-online.target"];
    serviceConfig = {
      RuntimeMaxSec = "${toString (5 * 60 * 60)}s";
      Restart = "always";
    };
  };

  environment.systemPackages = [refresh-qasaid];

  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      path = liquidsoapDirectory;
      mode = "0750";
      user = "liquidsoap";
      group = "liquidsoap";
      age = "1h";
    })
  ];

  services.icecast = {
    enable = true;
    hostname = "radio.kmein.de";
    admin.password = "hackme";
    listen.port = 6457;
    extraConf = ''
      <authentication>
        <source-password>${icecastPassword}</source-password>
      </authentication>
    '';
  };

  services.nginx.virtualHosts."radio.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString config.services.icecast.listen.port}";
  };

  niveum.passport.services = [
    {
      title = "Radio";
      link = "https://radio.kmein.de";
      description = "broadcasts a few little (and mostly useless) web-radio stations.";
    }
  ];
}
