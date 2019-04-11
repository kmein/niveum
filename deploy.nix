{ catullus-ssh ? "root@catullus.r"
, scardanelli-ssh ? "root@scardanelli.r:22022"
, homeros-ssh ? "root@homeros.r:22022"
}:
let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = name: path: lib.evalSource [{
    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs-channels;
      # ref = "6a3f5bcb061e1822f50e299f5616a0731636e4e7"; # 18.09
      ref = builtins.readFile ./NIXPKGS_VERSION;
    };
    nix-writers.git = {
      url = https://cgit.krebsco.de/nix-writers/;
      ref = "4d0829328e885a6d7163b513998a975e60dd0a72";
    };
    # stockholm.git = {
    #   url = https://cgit.krebsco.de/stockholm;
    #   ref = "7e1b197dab13d024ba491c96dc959306324943c0";
    # };
    system.file = toString path;
    art.file = toString ./art;
    lib.file = toString ./lib;
    packages.file = toString ./packages;
    systems.file = toString ./systems;
    configs.file = toString ./configs;
    dot.file = toString ./dot;
    modules.file = toString ./modules;

    nixos-config.symlink = "system/configuration.nix";
    # secrets.pass = {
    #   dir = toString ~/.password-store;
    #   name = name;
    # };
  }];

  systems.scardanelli = pkgs.krops.writeDeploy "deploy-scardanelli" {
    source = source "scardanelli" ./systems/scardanelli;
    target = scardanelli-ssh;
  };

  systems.homeros = pkgs.krops.writeDeploy "deploy-homeros" {
    source = source "homeros" ./systems/homeros;
    target = homeros-ssh;
  };

  systems.catullus = pkgs.krops.writeDeploy "deploy-catullus" {
    source = source "catullus" ./systems/catullus;
    target = catullus-ssh;
  };
in systems // {
  all = pkgs.writeScript "deploy-all" (lib.concatStringsSep "\n" (lib.attrValues systems));
}
