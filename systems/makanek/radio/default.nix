{ lib, pkgs, config, ... }:
let
  icecastPassword = "hackme";
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

    echo "annotate:title=\"$title\",album=\"$poem_url\",artist=\"$author\":$poem_file"
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

    printf "annotate:title=\"%s\",album=\"%s\",artist=\"%s\":$base/%s\n" \
      "$(${pkgs.htmlq}/bin/htmlq --text '.ce_text h1' -f "$html")" \
      "$base/$poem" \
      "$(${pkgs.htmlq}/bin/htmlq --text 'h1 + p em' -f "$html")" \
      "$(${pkgs.htmlq}/bin/htmlq 'audio source' --attribute src -f "$html")"
  '';
in {
  # https://github.com/savonet/liquidsoap/issues/1043#issuecomment-593354427
  services.liquidsoap.streams.radio = pkgs.writeText "lyrikline.liq" ''
    set("protocol.external.curl","${pkgs.curl}/bin/curl")

    def random_lyrikline() =
      uri = list.hd(default="", get_process_lines("${lyrikline-poem}"))
      request.create(uri, persistent=true)
    end

    def random_stavenhagen() =
      uri = list.hd(default="", get_process_lines("${stavenhagen-poem}"))
      request.create(uri, persistent=true)
    end

    lyrikline = mksafe(audio_to_stereo(request.dynamic(random_lyrikline)))

    output.icecast(
      mount = '/lyrikline.ogg',
      port = ${toString config.services.icecast.listen.port},
      password = "${icecastPassword}",
      description = "lyrikline. listen to the poet (unofficial)",
      %vorbis(quality = 1),
      lyrikline
    )

    stavenhagen = mksafe(audio_to_stereo(request.dynamic(random_stavenhagen)))

    output.icecast(
      mount = '/lyrik.ogg',
      port = ${toString config.services.icecast.listen.port},
      password = "${icecastPassword}",
      description = "Lyrik für alle – Neue Lust auf Lyrik | www.deutschelyrik.de",
      %vorbis(quality = 1),
      stavenhagen
    )
  '';

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
}
