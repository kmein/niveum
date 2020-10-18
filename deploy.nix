let
  gitFromJson = path:
    let object = importJson path;
    in {
      inherit (object) url;
      ref = object.rev;
    };
  krops = builtins.fetchGit (gitFromJson .versions/krops.json);
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" { };
  importJson = (import <nixpkgs> { }).lib.importJSON;

  regularSystem = path: name: {
    source = lib.evalSource [{
      niveum.file = toString ./.;
      system.file = toString path;
      nixos-config.symlink = "system/configuration.nix";

      nixpkgs.git = gitFromJson .versions/nixpkgs.json;
      nixos-unstable.git = gitFromJson .versions/nixpkgs-unstable.json;
      home-manager.git = gitFromJson .versions/home-manager.json;
      stockholm.git = gitFromJson .versions/stockholm.json;
      retiolum.git = gitFromJson .versions/retiolum.json;
      system-secrets.pass = {
        dir = toString ~/.password-store/systems;
        inherit name;
      };
      secrets.pass = {
        dir = toString ~/.password-store;
        name = "shared";
      };
    }];
    target = "root@${name}:22022";
  };
  inherit (pkgs.krops) writeDeploy;
in {
  scardanelli = writeDeploy "deploy-scardanelli"
    (regularSystem systems/scardanelli "scardanelli");
  homeros =
    writeDeploy "deploy-homeros" (regularSystem systems/homeros "homeros");
  wilde = writeDeploy "deploy-wilde" (regularSystem systems/wilde "wilde");

  catullus =
    writeDeploy "deploy-catullus" (regularSystem systems/catullus "catullus");
}
