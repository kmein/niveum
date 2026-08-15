{ config, pkgs, ... }:
let
  domain = "rad.${pkgs.lib.niveum.domain}";
  explorer = pkgs.radicle-explorer.withConfig {
    preferredSeeds = [
      {
        hostname = domain;
        port = 443;
        scheme = "https";
      }
    ];
  };
in
{
  services.radicle = {
    enable = true;
    # NID: did:key:z6MkjwDHFzqkFXKcQyRffR1nnAezSN874WHbYwhXg49KR7gF
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFFzklaW4LmpaoY8kLQ/1z7gAB7KrLj7C02exLYpeU3c";
    privateKey = config.age.secrets.radicle-key.path;
    node.openFirewall = true;
    httpd.enable = true;

    settings = {
      publicExplorer = "https://${domain}/nodes/$host/$rid$path";
      preferredSeeds = [
        "z6MksZTwWbCREEE8DCroVwGFX48BTSSsgtBaoJ8sExvjk8Lf@radicle.nomath.org:8776"
      ];
      node = {
        alias = "makanek";
        externalAddresses = [
          "${domain}:${toString config.services.radicle.node.listenPort}"
        ];
        seedingPolicy.default = "block";
      };
      web.pinned.repositories = [
        "rad:z4MdDeiMDiq3z63QmM31jskx7kt1L" # niveum
        "rad:z4XsRZGnkWvzMzcYypqvZmsL3BUv4" # awebframework
      ];
    };
  };

  age.secrets.radicle-key.file = ../../secrets/radicle-key.age;

  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    locations = {
      "/" = {
        root = explorer;
        tryFiles = "$uri $uri/ /index.html";
      };
      "/api/".proxyPass =
        "http://${config.services.radicle.httpd.listenAddress}:${toString config.services.radicle.httpd.listenPort}";
    };
  };

  services.restic.backups.niveum.paths = [ "/var/lib/radicle" ];

  niveum.passport.services = [
    {
      link = domain;
      title = "Radicle";
      description = ''
        seeds a couple of <tt>git</tt> repos over the peer-to-peer
        <a href="https://radicle.xyz">Radicle</a> network.
      '';
    }
  ];
}
