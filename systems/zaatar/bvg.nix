{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) serveHtml;
  stations = [
    900068204 # A/M
    900068302 # KAS
    900068203 # B-P
  ];
  fahrplan = pkgs.writeText "fahrplan.html" ''
    <!DOCTYPE html>
    <title>Fahrplan</title>
    <link
      rel="icon"
      type="image/x-icon"
      href="https://mobil.bvg.de/Fahrinfo/img/ua_xhtml/logo.gif"
    />
    <style>
      body {
        margin: 0;
        --bvg-yellow: #f0d722;
      }
      #fahrplan {
        display: flex;
        height: 100vh;
        width: 100%;
        flex-direction: row;
      }
      #fahrplan iframe {
        flex-grow: 1;
        border: none;
      }
      #fahrplan iframe + iframe {
        border-left: 2px solid var(--bvg-yellow);
      }
    </style>
    <body>
      <div id="fahrplan">
        ${lib.concatMapStrings (station: ''
          <iframe scrolling="no" src="https://mobil.bvg.de/Fahrinfo/bin/stboard.bin/dox?ld=0.1&input=${toString station}&boardType=depRT&start=yes"></iframe>
        '') stations}
      </div>
    </body>
  '';
in
{
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.nginx.virtualHosts."bvg.kmein.r" = {
    locations."/".extraConfig = serveHtml fahrplan pkgs;
  };
}
