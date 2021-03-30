let
  inherit (import ./lib/default.nix) sshPort;

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

      nixpkgs.git = gitFromJson .versions/nixpkgs.json // { shallow = true; };
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
    target = "root@${address}:${toString sshPort}";
  };
  inherit (pkgs.krops) writeDeploy;
in {
  zaatar = writeDeploy "deploy-zaatar" (regularSystem {
    path = systems/zaatar;
    name = "zaatar";
    address = "zaatar.r";
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
  }) // {
    buildTarget = "${builtins.getEnv "USER"}@localhost/${builtins.getEnv "HOME"}/.cache/krops";
  };
  makanek = writeDeploy "deploy-makanek" (regularSystem {
    path = systems/makanek;
    name = "makanek";
    address = "makanek.r";
  });
  manakish = writeDeploy "deploy-manakish" (regularSystem {
    path = systems/manakish;
    name = "manakish";
    address = "manakish.r";
  });
}
