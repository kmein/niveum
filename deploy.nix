let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
    ref = "v1.14.0";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};
  importJson = (import <nixpkgs> {}).lib.importJSON;

  niveum = {
    lib.file = toString ./lib;
    packages.file = toString ./packages;
    configs.file = toString ./configs;
    dot.file = toString ./dot;
    modules.file = toString ./modules;
    nixos-config.symlink = "system/configuration.nix";

    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs-channels;
      ref = (importJson ./nixpkgs.json).rev;
    };
    nixos-unstable.git = {
      url = https://github.com/NixOS/nixpkgs-channels;
      ref = "nixos-unstable";
    };
    home-manager.git = {
      url = https://github.com/rycee/home-manager;
      ref = "2ccbf43";
    };
    stockholm.git = {
      url = https://cgit.krebsco.de/stockholm;
      ref = "1340e3fb";
    };
  };

  regularSystem = path: name: extras: {
    source = lib.evalSource [
      (niveum // extras // {
        system.file = toString path;
        secrets.pass = {
          dir = toString ~/.password-store/systems;
          inherit name;
        };
      })
    ];
    target = "root@${name}:22022";
  };
in {
  scardanelli = pkgs.krops.writeDeploy "deploy-scardanelli" (regularSystem systems/scardanelli "scardanelli" { art.file = toString ./art; });
  homeros = pkgs.krops.writeDeploy "deploy-homeros" (regularSystem systems/homeros "homeros" { art.file = toString ./art; });
  wilde = pkgs.krops.writeDeploy "deploy-wilde" (regularSystem systems/wilde "wilde" { art.file = toString ./art; });

  catullus = pkgs.krops.writeDeploy "deploy-catullus" (regularSystem systems/catullus "catullus" {});
}
