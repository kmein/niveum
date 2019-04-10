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
      ref = "22a606e20d662e2575552ab9b5e7c31aa8331e0e";
    };
    # stockholm.git = {
    #   url = https://cgit.krebsco.de/stockholm;
    #   ref = "9b2355521f8447e7da3af30bce8fb7ba6f83ed69";
    # };
    system.file = toString path;
    niveum.file = toString ../.;
    nixos-config.symlink = "system/physical.nix";
    # secrets.pass = {
    #   dir = toString ~/.password-store;
    #   name = name;
    # };
  }];

  systems.scardanelli = pkgs.krops.writeDeploy "deploy-scardanelli" {
    source = source "scardanelli" ./scardanelli;
    target = scardanelli-ssh;
  };

  systems.homeros = pkgs.krops.writeDeploy "deploy-homeros" {
    source = source "homeros" ./homeros;
    target = homeros-ssh;
  };

  systems.catullus = pkgs.krops.writeDeploy "deploy-catullus" {
    source = source "catullus" ./catullus;
    target = catullus-ssh;
  };
in systems // {
  all = pkgs.writeScript "deploy-all" (lib.concatStringsSep "\n" (lib.attrValues systems));
}
