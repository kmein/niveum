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

  regularSystem = { path, name, address }: {
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
        dir = toString ~/.password-store;
        name = "systems/${name}";
      };
      secrets.pass = {
        dir = toString ~/.password-store;
        name = "shared";
      };
    }];
    target = "root@${address}:22022";
  };
  inherit (pkgs.krops) writeDeploy;
in {
  scardanelli = writeDeploy "deploy-scardanelli" (regularSystem {
    path = systems/scardanelli;
    name = "scardanelli";
    address = "scardanelli.r";
  });
  homeros = writeDeploy "deploy-homeros" (regularSystem {
    path = systems/homeros;
    name = "homeros";
    address = "homeros.r";
  });
  wilde = writeDeploy "deploy-wilde" (regularSystem {
    path = systems/wilde;
    name = "wilde";
    address = "wilde.r";
  });
  toum = writeDeploy "deploy-toum" (regularSystem {
    path = systems/toum;
    name = "toum";
    address = "toum.r";
  });
}
