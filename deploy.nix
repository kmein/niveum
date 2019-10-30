let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
    ref = (importJson _versions/krops.json).rev;
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};
  importJson = (import <nixpkgs> {}).lib.importJSON;

  regularSystem = path: name: {
    source = lib.evalSource [
      {
        niveum.file = toString ./.;
        system.file = toString path;
        nixos-config.symlink = "system/configuration.nix";

        nixpkgs.git = {
          url = "https://github.com/NixOS/nixpkgs-channels";
          ref = (importJson _versions/nixpkgs.json).rev;
        };
        nixos-unstable.git = {
          url = "https://github.com/NixOS/nixpkgs-channels";
          ref = (importJson _versions/nixpkgs-unstable.json).rev;
        };
        home-manager.git = {
          url = "https://github.com/rycee/home-manager";
          ref = (importJson _versions/home-manager.json).rev;
        };
        stockholm.git = {
          url = "https://cgit.krebsco.de/stockholm";
          ref = (importJson _versions/stockholm.json).rev;
        };
        secrets.pass = {
          dir = toString ~/.password-store/systems;
          inherit name;
        };
        shared-secrets.pass = {
          dir = toString ~/.password-store;
          name = "shared";
        };
      }
    ];
    target = "root@${name}:22022";
  };
in {
  scardanelli = pkgs.krops.writeDeploy "deploy-scardanelli" (regularSystem systems/scardanelli "scardanelli");
  homeros = pkgs.krops.writeDeploy "deploy-homeros" (regularSystem systems/homeros "homeros");
  wilde = pkgs.krops.writeDeploy "deploy-wilde" (regularSystem systems/wilde "wilde");

  catullus = pkgs.krops.writeDeploy "deploy-catullus" (regularSystem systems/catullus "catullus");
}
