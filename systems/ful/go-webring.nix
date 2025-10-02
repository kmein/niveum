{ config, niveumPackages ,... }:
let
  port = 2857;
in
{
  services.go-webring = {
    enable = true;
    host = "dichtungsring.kmein.de";
    listenAddress = "127.0.0.1:${toString port}";
    package = niveumPackages.go-webring;
    members = [
      { username = "meteora"; site = "meteora.xn--kiern-0qa.de"; }
      { username = "huldra"; site = "huldras-halbtraum.com"; }
    ];
    homePageTemplate = ''
      <!DOCTYPE html>
      <html lang="de">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Dichtungsring</title>
      </head>
      <body>
        <h1>Willkommen beim Dichtungs-Ring</h1>
        <section id="members">
        {{ . }}
        </section>
      </body>
      </html>
    '';
  };

  services.nginx.virtualHosts."dichtungsring.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://${config.services.go-webring.listenAddress}";
  };
}
