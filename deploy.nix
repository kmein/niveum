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
      ref = "07d4df5";
    };
    home-manager.git = {
      url = https://github.com/rycee/home-manager;
      ref = "2ccbf43";
    };
    stockholm.git = {
      url = https://cgit.krebsco.de/stockholm;
      ref = "421a9792";
    };
  };

  regularSystem = path: name: {
    source = lib.evalSource [
      (niveum // {
        system.file = toString path;
        secrets.pass = {
          dir = toString ~/.password-store/systems;
          inherit name;
        };
        shared-secrets.pass = {
          dir = toString ~/.password-store;
          name = "shared";
        };
      })
    ];
    target = "root@${name}:22022";
  };
in {
  scardanelli = pkgs.krops.writeDeploy "deploy-scardanelli" (regularSystem systems/scardanelli "scardanelli");
  homeros = pkgs.krops.writeDeploy "deploy-homeros" (regularSystem systems/homeros "homeros");
  wilde = pkgs.krops.writeDeploy "deploy-wilde" (regularSystem systems/wilde "wilde");

  catullus = pkgs.krops.writeDeploy "deploy-catullus" (regularSystem systems/catullus "catullus" {});
}
