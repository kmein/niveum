let
  gitFromJson = path:
  let object = importJson path; in {
    inherit (object) url;
    ref = object.rev;
  };
  krops = builtins.fetchGit (gitFromJson .versions/krops.json);
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};
  importJson = (import <nixpkgs> {}).lib.importJSON;

  regularSystem = path: name: {
    source = lib.evalSource [
      {
        niveum.file = toString ./.;
        system.file = toString path;
        nixos-config.symlink = "system/configuration.nix";

        nixpkgs.git = gitFromJson .versions/nixpkgs.json;
        nixos-unstable.git = gitFromJson .versions/nixpkgs-unstable.json;
        home-manager.git = gitFromJson .versions/home-manager.json;
        stockholm.git = gitFromJson .versions/stockholm.json;
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
