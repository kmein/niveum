{ pkgs, ... }:
let
  backupLocation = "/var/lib/codimd-backup";
  stateLocation = "/var/lib/codimd/state.sqlite";
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
      ${pkgs.sqlite}/bin/sqlite3 -csv ${stateLocation} "select shortid, alias, ownerId, content from Notes" \
      | ${pkgs.writers.writePython3 "hedgedoc-csv-to-fs.py" {} ''
        import csv
        import pathlib
        import sys

        reader = csv.reader(
            (line.decode("utf-8") for line in sys.stdin.buffer.readlines()),
            dialect="unix"
        )
        for row in reader:
            try:
                id, alias, ownerId, content = row

                user_directory = pathlib.Path(ownerId)
                user_directory.mkdir(exist_ok=True)

                file_path = user_directory / ((alias if alias else id) + ".md")
                file_path.write_text(content)

                sys.stderr.write(f"✔ {file_path}\n")
            except ValueError:
                sys.stderr.write(
                    f"row {reader.line_num} does not have the correct number of fields"
                )
                continue
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
