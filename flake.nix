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
    source = name: {
      niveum.file = toString ./.;
      nixos-config.symlink = "niveum/systems/${name}/configuration.nix";
      system-secrets.pass = {
        dir = toString ~/.password-store;
        name = "systems/${name}";
      };
      secrets.pass = {
        dir = toString ~/.password-store;
        name = "shared";
      };
    } // nixpkgs.lib.mapAttrs' (name: value: {
      inherit name;
      value.git = {
        url = {
          # having to declare the git upstream urls here is suboptimal, but the inputs don't remember where they're from
          home-manager = "https://github.com/nix-community/home-manager";
          menstruation-backend = "https://github.com/kmein/menstruation.rs";
          menstruation-telegram = "https://github.com/kmein/menstruation-telegram";
          nix-writers = "https://cgit.krebsco.de/nix-writers";
          nixpkgs = "https://github.com/NixOS/nixpkgs";
          nixpkgs-unstable = "https://github.com/NixOS/nixpkgs";
          recht = "https://github.com/kmein/recht";
          retiolum = "https://github.com/krebs/retiolum";
          scripts = "https://github.com/kmein/scripts";
          stockholm = "https://cgit.lassul.us/stockholm";
          telebots = "https://github.com/kmein/telebots";
          tinc-graph = "https://github.com/kmein/tinc-graph";
          traadfri = "https://github.com/kmein/traadfri";
          tuna = "https://github.com/kmein/tuna";
        }.${name};
        ref = value.rev;
        shallow = true;
      };
    }) (nixpkgs.lib.filterAttrs (name: _: !builtins.elem name [ "flake-utils" "krops" "self" ]) inputs);
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
          program = deployScriptFor { inherit name; host = "${name}.r"; };
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
