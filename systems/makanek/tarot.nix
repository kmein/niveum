{
  config,
  pkgs,
  lib,
  ...
}:
let
  tarotPort = 7407;
  tarotFiles = pkgs.fetchzip {
    url = "https://c.krebsco.de/tarot.zip";
    sha256 = "0jl5vdwlj17pqp94yj02xgsb1gyvs9i08m83kac0jdnhfjl2f75a";
    stripRoot = false;
  };
  tarotKey = builtins.fetchurl {
    url = "http://c.krebsco.de/tarot.pdf";
    sha256 = "1n2m53kjg2vj9dbr70b9jrsbqwdfrcb48l4wswn21549fi24g6dx";
  };
in
{
  systemd.services.tarot = {
    enable = true;
    serviceConfig.Type = "simple";
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = pkgs.writers.writePython3 "tarot-server" {
      libraries = py: [ py.pillow py.flask ];
    } ''
      from flask import Flask, send_file
      from pathlib import Path
      from random import choice, randint
      from io import BytesIO
      from PIL import Image

      app = Flask(__name__)
      TAROT_DIR = Path("${tarotFiles}")


      @app.route("/")
      def tarot():
          card_path = choice(list(TAROT_DIR.glob("*")))

          with Image.open(card_path) as img:
              if randint(0, 1):
                  img = img.rotate(180)
              buf = BytesIO()
              img.save(buf, format="JPEG")
              buf.seek(0)
              return send_file(
                  buf,
                  mimetype='image/jpeg',
                  as_attachment=False
              )


      if __name__ == "__main__":
          app.run(port=${toString tarotPort})
    '';
  };

  niveum.passport.services = [
    rec {
      link = "https://tarot.kmein.de";
      title = "Tarot";
      description = "draws Tarot cards for you. See <a href=\"${link}/files/key.pdf\">here</a> for information on how to interpret them.";
    }
  ];

  services.nginx.virtualHosts."tarot.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString tarotPort}/";
      "/files/" = {
        root = pkgs.linkFarm "tarot" [
          {
            name = "files/key.pdf";
            path = tarotKey;
          }
          {
            name = "files/cards";
            path = tarotFiles;
          }
        ];
        extraConfig = ''
          autoindex on;
          charset UTF-8;
        '';
      };
    };
  };
}
