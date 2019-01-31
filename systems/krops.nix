let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = path: lib.evalSource [{
    nixpkgs.git = {
      ref = "6a3f5bcb061e1822f50e299f5616a0731636e4e7"; # 18.09
      url = https://github.com/NixOS/nixpkgs-channels;
    };
    system.file = toString path;
    nixos-config.symlink = "system/configuration.nix";
  }];

  systems.catullus = pkgs.krops.writeDeploy "deploy-catullus" {
    source = source ./catullus;
    target = "root@catullus.local";
  };
in systems // {
  all = pkgs.writeScript "deploy-all" (lib.concatStringsSep "\n" (lib.attrValues systems));
}
