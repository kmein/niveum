let
  krops = builtins.fetchGit (gitFromJson .versions/krops.json);
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  importJson = (import <nixpkgs> {}).lib.importJSON;
  gitFromJson = path:
    let
      object = importJson path;
    in {
      inherit (object) url;
      ref = object.rev;
    };

  source = name: {
    niveum.file = toString ./.;
    nixos-config.symlink =  "niveum/systems/${name}/configuration.nix";

    nixpkgs.git = gitFromJson .versions/nixpkgs.json // { shallow = true; };
    nixpkgs-unstable.git = gitFromJson .versions/nixpkgs-unstable.json // { shallow = true; };
    home-manager.git = gitFromJson .versions/home-manager.json;
    stockholm.git = gitFromJson .versions/stockholm.json;
    nix-writers.git = gitFromJson .versions/nix-writers.json;
    retiolum.git = gitFromJson .versions/retiolum.json;
    nixpkgs-mozilla.git = gitFromJson .versions/nixpkgs-mozilla.json;
    system-secrets.pass = {
      dir = toString ~/.password-store;
      name = "systems/${name}";
    };
    secrets.pass = {
      dir = toString ~/.password-store;
      name = "shared";
    };
  };

  system = {name, host}: let inherit (import ./lib/default.nix) sshPort; in pkgs.krops.writeDeploy "deploy-${name}" {
    source = lib.evalSource [ (source name) ];
    target = "root@${host}:${toString sshPort}";
  };
in {
  zaatar = system { name = "zaatar"; host = "zaatar.r"; };
  kabsa = system { name = "kabsa"; host = "kabsa.r"; };
  makanek = system { name = "makanek"; host = "makanek.r"; };
  manakish = system { name = "manakish"; host = "manakish.r"; };
}
