{pkgs, ...}: let
  port = 9610;
  web-socket-sink-src = "${<scripts>}/grimm-scroller";
  web-socket-sink = pkgs.callPackage web-socket-sink-src {};
  lemmata = "${web-socket-sink-src}/dwb-compact.json";
in {
  systemd.services.grimm-ws = {
    wantedBy = ["multi-user.target"];
    script = "${web-socket-sink}/bin/web-socket-sink --host 0.0.0.0 --port ${toString port} < ${lemmata}";
    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
    };
  };

  services.nginx.virtualHosts."grimm.kmein.de" = {
    enableACME = false;
    forceSSL = false;
    locations = {
      "/".root = pkgs.linkFarm "grimm" [
        {
          name = "index.html";
          path = "${web-socket-sink-src}/wclient.html";
        }
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [port];
}
