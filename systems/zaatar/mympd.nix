{pkgs, ...}: let
  inherit (import <niveum/lib>) tmpfilesConfig;
in {
  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      mode = "0755";
      user = "mympd";
      group = "mympd";
      path = "/var/lib/mympd";
    })
    (tmpfilesConfig {
      type = "d";
      mode = "0755";
      user = "mympd";
      group = "mympd";
      age = "1d";
      path = "/var/cache/mympd";
    })
    (tmpfilesConfig {
      type = "L+";
      mode = "0644";
      user = "mympd";
      group = "mympd";
      path = "/var/lib/mympd/config/http_port";
      argument = pkgs.writeText "port" "8764";
    })
    (tmpfilesConfig {
      type = "L+";
      mode = "0644";
      user = "mympd";
      group = "mympd";
      path = "/var/lib/mympd/config/ssl";
      argument = pkgs.writeText "ssl" "false";
    })
  ];

  users.users.mympd = {
    isSystemUser = true;
    group = "mympd";
  };
  users.groups.mympd = {};

  systemd.services.mympd = {
    wantedBy = ["multi-user.target"];
    after = ["mpd.service"];
    script = "${pkgs.mympd}/bin/mympd";
    environment = {
      MYMPD_HTTP_PORT = "8764";
    };
    serviceConfig = {
      Restart = "always";
      User = "mympd";
      Group = "mympd";
    };
  };
}
