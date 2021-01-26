{ pkgs, config, lib, ... }:
let
  tunerHTML = pkgs.callPackage <niveum/packages/tuner.nix> {
    playlists = import <niveum/lib/playlists.nix> { inherit lib; };
  };
in
{
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts.default = {
      locations."= /tuner".extraConfig = ''
        default_type text/html;
        alias ${tunerHTML};
      '';
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
