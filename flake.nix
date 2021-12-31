{
  description = "niveum: packages, modules, systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    krops = {
      url = "github:Mic92/krops";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    stockholm = {
      url = "git+https://cgit.lassul.us/stockholm";
      flake = false;
    };
    nix-writers = {
      url = "git+https://cgit.krebsco.de/nix-writers";
      flake = false;
    };
    retiolum = {
      url = "github:krebs/retiolum";
      flake = false;
    };

    menstruation-telegram = { url = "github:kmein/menstruation-telegram"; flake = false; };
    traadfri = { url = "github:kmein/traadfri"; flake = false; };
    menstruation-backend = { url = "github:kmein/menstruation.rs"; flake = false; };
    telebots = { url = "github:kmein/telebots"; flake = false; };
    recht = { url = "github:kmein/recht"; flake = false; };
    tuna = { url = "github:kmein/tuna"; flake = false; };
    scripts = { url = "github:kmein/scripts"; flake = false; };
    tinc-graph = { url = "github:kmein/tinc-graph"; flake = false; };
  };

  outputs =
  { self
  , flake-utils
  , home-manager
  , krops
  , menstruation-backend
  , menstruation-telegram
  , telebots
  , traadfri
  , recht
  , tuna
  , scripts
  , tinc-graph
  , nix-writers
  , nixpkgs
  , nixpkgs-unstable
  , retiolum
  , stockholm
  }@inputs:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    # having to declare the git upstream urls here is suboptimal, but the inputs don't remember where they're from
    source = name: {
      niveum.file = toString ./.;
      nixos-config.symlink = "niveum/systems/${name}/configuration.nix";
      nixpkgs.git = { url = "https://github.com/NixOS/nixpkgs"; ref = nixpkgs.rev; shallow = true; };
      nixpkgs-unstable.git = { url = "https://github.com/NixOS/nixpkgs"; ref = nixpkgs-unstable.rev; shallow = true; };
      home-manager.git = { url = "https://github.com/nix-community/home-manager"; ref = home-manager.rev; };
      stockholm.git = { url = "https://cgit.lassul.us/stockholm"; ref = stockholm.rev; };
      nix-writers.git = { url = "https://cgit.krebsco.de/nix-writers"; ref = nix-writers.rev; };
      retiolum.git = { url = "https://github.com/krebs/retiolum"; ref = retiolum.rev; };

      traadfri.git = { url = "https://github.com/kmein/traadfri"; ref = tuna.rev; };
      tuna.git = { url = "https://github.com/kmein/tuna"; ref = tuna.rev; };
      telebots.git = { url = "https://github.com/kmein/telebots"; ref = telebots.rev; };
      recht.git = { url = "https://github.com/kmein/recht"; ref = recht.rev; };
      menstruation-telegram.git = { url = "https://github.com/kmein/menstruation-telegram"; ref = menstruation-telegram.rev; };
      menstruation-backend.git = { url = "https://github.com/kmein/menstruation.rs"; ref = menstruation-backend.rev; };
      scripts.git = { url = "https://github.com/kmein/scripts"; ref = scripts.rev; };
      tinc-graph.git = { url = "https://github.com/kmein/tinc-graph"; ref = tinc-graph.rev; };

      system-secrets.pass = {
        dir = toString ~/.password-store;
        name = "systems/${name}";
      };
      secrets.pass = {
        dir = toString ~/.password-store;
        name = "shared";
      };
    };
    deployScriptFor = {name, host}: let inherit (import ./lib/default.nix) sshPort; in toString (krops.packages.${system}.writeDeploy "deploy-${name}" {
      source = krops.lib.evalSource [ (source name) ];
      target = "root@${host}:${toString sshPort}";
    });
  in {
    apps.${system} = let
      forSystems = f: builtins.listToAttrs (map f (builtins.attrNames (builtins.readDir ./systems)));
      deployScripts = forSystems (name: {
        name = "deploy-${name}";
        value = {
          type = "app";
          program = deployScriptFor { inherit name; host = "${system}.r"; };
        };
      });
      ciScripts = forSystems (name: {
        name = "build-${name}";
        value = {
          type = "app";
          program = import ./ci.nix { inherit name system inputs; };
        };
      });
    in deployScripts // ciScripts;
  };
}
