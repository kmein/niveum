{
  pkgs,
  config,
  ...
}: {
  age.secrets.stw-berlin-card-code.file = ../secrets/stw-berlin-card-code.age;

  systemd.services.stw-berlin = {
    enable = true;
    wants = ["network-online.target"];
    startAt = "weekly";
    serviceConfig = {
      User = config.users.users.me.name;
      Group = config.users.users.me.group;
      WorkingDirectory = "/home/kfm/cloud/nextcloud/Uni/Meta/Mensa";
      LoadCredential = [
        "password:${config.age.secrets.stw-berlin-card-code.path}"
      ];
    };
    script = ''
      KARTEN_ID=8071859
      PASSWORT=$(cat "$CREDENTIALS_DIRECTORY"/password)

      endpoint=https://ks.stw.berlin:4433/TL1/TLM/KASVC
      authorization_header='Authorization: Basic S0FTVkM6ekt2NXlFMUxaVW12VzI5SQ=='

      get_auth_token() {
        ${pkgs.curl}/bin/curl -sSL "$endpoint/LOGIN?karteNr=$KARTEN_ID&format=JSON&datenformat=JSON" \
          -X POST \
          -H "$authorization_header" \
          --data-raw '{"BenutzerID":"'$KARTEN_ID'","Passwort":"'$PASSWORT'"}' \
          | ${pkgs.jq}/bin/jq -r '.[0].authToken|@uri'
      }


      get_transactions() {
        ${pkgs.curl}/bin/curl -sSL "$endpoint/TRANS?format=JSON&authToken=$(get_auth_token)&karteNr=$KARTEN_ID&datumVon=12.02.2018&datumBis=$(date -d tomorrow +%d.%m.%Y)" \
          -H "$authorization_header" \
          | ${pkgs.jq}/bin/jq
      }

      get_items() {
        ${pkgs.curl}/bin/curl -sSL "$endpoint/TRANSPOS?format=JSON&authToken=$(get_auth_token)&karteNr=$KARTEN_ID&datumVon=12.02.2018&datumBis=$(date -d tomorrow +%d.%m.%Y)" \
          -H "$authorization_header" \
          | ${pkgs.jq}/bin/jq
      }

      get_transactions > transactions-$(date -I).json
      get_items > items-$(date -I).json
    '';
  };
}
