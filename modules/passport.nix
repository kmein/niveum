{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.niveum.passport;
  sortOn = a: lib.sort (as1: as2: lib.lessThan (lib.getAttr a as1) (lib.getAttr a as2));
  css = ''
    body {
      margin: 0;
      font-family: "Fira Sans Condensed", sans-serif;
    }

    main {
      margin: 0 auto;
      display: grid;
      grid-template-columns: 1fr 3fr;
      grid-gap: 2em;
    }
    @media only screen and (max-width: 768px) {
      main {
        grid-template-columns: 1fr;
      }
    }

    footer, section {
      padding: 1em;
    }

    footer {
      text-align: center;
    }

    dl {
      border: 3px double #ccc;
      padding: 0.5em;
    }
    dt {
      float: left;
      clear: left;
      width: 200px;
      text-align: right;
      font-weight: bold;
      margin-right: 1em;
      margin-bottom: 1em;
    }
    dd {
      margin: 0 0 0 110px;
      padding: 0 0 0.5em 0;
      margin-bottom: 1em;
    }
  '';
in
with lib;
{
  options.niveum.passport = {
    enable = mkEnableOption "server passport";

    introductionHTML = mkOption { type = types.str; };

    virtualHost = mkOption {
      type = types.str;
    };

    services = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            title = mkOption { type = types.str; };
            link = mkOption {
              type = types.nullOr types.str;
              default = null;
            };
            description = mkOption {
              type = types.str;
              default = "";
            };
          };
        }
      );
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    services.nginx.enable = true;

    services.nginx.virtualHosts."${cfg.virtualHost}".locations."/passport".extraConfig = ''
      default_type "text/html";
      root ${
        pkgs.linkFarm "www" [
          {
            name = "passport/index.html";
            path = pkgs.writeText "index.html" ''
              <!doctype html>
              <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <title>${config.networking.hostName} passport</title>
                <style>${css}</style>
              </head>
              <body>
                <main>
                  <section id="server">
                  <h1>${config.networking.hostName}</h1>
                  ${cfg.introductionHTML}
                  </section>

                  <section id="services">
                  <h2>Services</h2>
                  <dl>
                  ${lib.strings.concatMapStringsSep "\n" (service: ''
                    <dt>
                    ${lib.optionalString (service.link != null) "<a href=\"${service.link}\">"}
                    ${service.title}
                    ${lib.optionalString (service.link != null) "</a>"}
                    </dt>
                    <dd>
                    ${service.description}
                    </dd>
                  '') (sortOn "title" cfg.services)}
                  </dl>
                  </section>
                </main>

                <footer>
                  <tt>${config.networking.hostName}</tt> is part of the <i><a href="https://github.com/kmein/niveum/tree/master/systems/${config.networking.hostName}">niveum</a></i> network.
                </footer>
              </body>
            '';
          }
        ]
      };
      index index.html;
    '';
  };
}
