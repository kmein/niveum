{ pkgs, ... }:
let
  port = 9090;
  scrabbleDirectory = "/var/lib/xanado";
in
{
  users.extraUsers.scrabble = {
    isSystemUser = true;
    group = "scrabble";
    home = scrabbleDirectory;
    createHome = true;
  };
  users.extraGroups.scrabble = {};

  systemd.services.scrabble = {
    wantedBy = ["multi-user.target"];
    enable = true;
    preStart = "npm install @cdot/xanado";
    path = [ pkgs.nodejs ];
    script = ''
      ${scrabbleDirectory}/node_modules/.bin/xanado --config ${(pkgs.formats.json {}).generate "config.json" {
        port = port;
        host = "localhost";
        game_defaults = {
          edition = "Deutsch_Scrabble";
          dictionary = "German";
        };
      }}
    '';
    serviceConfig = {
      User = "scrabble";
      Group = "scrabble";
      WorkingDirectory = scrabbleDirectory;
    };
  };


  services.nginx.virtualHosts."scrabble.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:${toString port}";
  };

  systemd.services.scrabble-fix = {
    startAt = "hourly";
    wantedBy = ["multi-user.target"];
    enable = false;
    script = ''
     ${pkgs.gnused}/bin/sed -i s/encadefrit/en/ sessions/*.json passwd.json"
    '';
    serviceConfig = {
      User = "scrabble";
      Group = "scrabble";
      WorkingDirectory = scrabbleDirectory;
    };
  };

  services.restic.backups.niveum.paths = [ scrabbleDirectory ];
}
