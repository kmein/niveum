{ pkgs, ... }:
let
  backupLocation = "/var/lib/codimd-backup";
  stateLocation = "/var/lib/codimd/state.sqlite";
  nixpkgs-unstable = import <nixpkgs-unstable> {};
in
{
  imports = [ <stockholm/krebs/3modules/permown.nix> ];

  services.nginx.virtualHosts."pad.xn--kiern-0qa.de" = {
    enableACME = true;
    addSSL = true;
    locations."/".extraConfig = ''
      client_max_body_size 4G;
      proxy_set_header Host $host;
      proxy_pass http://localhost:3091;
    '';
  };

  services.hedgedoc = {
    enable = true;
    configuration = {
      allowAnonymous = true;
      allowGravatar = false;
      allowFreeURL = true;
      db = {
        dialect = "sqlite";
        storage = stateLocation;
      };
      port = 3091;
    };
  };

  krebs.permown.${backupLocation} = { owner = "codimd"; group = "codimd"; umask = "0002"; };

  systemd.services.hedgedoc-backup = {
    description = "Hedgedoc backup service";
    script = ''
      ${nixpkgs-unstable.sqlite}/bin/sqlite3 -json ${stateLocation} "select shortid, alias, ownerId, content from Notes" \
      | ${pkgs.writers.writePython3 "hedgedoc-json-to-fs.py" {} ''
        import json
        import pathlib
        import sys

        for note in json.load(sys.stdin):
            user_directory = pathlib.Path()
            if note["ownerId"]:
                user_directory = pathlib.Path(note["ownerId"])
                user_directory.mkdir(exist_ok=True)
            file_path = user_directory / (
                (note["alias"] if note["alias"] else note["shortid"]) + ".md"
            )
            file_path.write_text(note["content"])
            print(f"✔ {file_path}", file=sys.stderr)
      ''}
    '';
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      User = "codimd";
      Group = "codimd";
      WorkingDirectory = backupLocation;
    };
  };
}
