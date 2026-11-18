{
  config,
  lib,
  pkgs,
  ...
}:
let
  # retiolum names get their certificates from krebs' ca.r, which only signs .r
  # and .w; every host trusts it through kartei.nixosModules.ca
  retiolumOnly =
    settings:
    lib.types.attrsOf (
      lib.types.submodule ({ name, ... }: { config = lib.mkIf (lib.hasSuffix ".r" name) settings; })
    );
in
{
  # CVE-2026-123633, CVE-2026-123007: unauthenticated RCE over HTTP/2, fixed in
  # 1.30.7 and 1.31.9; http2 comes back by itself once nixpkgs ships those
  imports = [
    {
      options.services.nginx.virtualHosts = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule {
            config.http2 =
              let
                v = config.services.nginx.package.version;
              in
              lib.mkIf (
                lib.versionOlder v "1.30.7" || (lib.versionAtLeast v "1.31" && lib.versionOlder v "1.31.9")
              ) (lib.mkForce false);
          }
        );
      };
    }
  ];

  options.services.nginx.virtualHosts = lib.mkOption {
    type = retiolumOnly {
      enableACME = lib.mkDefault true;
      # not addSSL: the challenge then lands in the plain-http server, which
      # carries none of the vhost's nginxAllow, and ca.r is not one of ours
      forceSSL = lib.mkDefault true;
    };
  };
  options.security.acme.certs = lib.mkOption {
    type = retiolumOnly { server = config.retiolum.ca.acmeURL; };
  };

  config = {
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
      # without an explicit default, nginx serves the lexicographically first vhost
      # to any unknown Host, including ones meant only for the overlays
      virtualHosts."_" = {
        default = lib.mkDefault true;
        rejectSSL = true;
        locations."/".return = "444";
      };
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = pkgs.lib.niveum.kieran.email;
    };
  };
}
