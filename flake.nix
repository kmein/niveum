{
  description = "niveum: packages, modules, systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
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

    # legacy
    menstruation-backend = {
      url = "github:kmein/menstruation.rs";
      flake = false;
    };
    menstruation-telegram = {
      url = "github:kmein/menstruation-telegram";
      flake = false;
    };
    nix-writers = {
      url = "git+https://cgit.krebsco.de/nix-writers";
      flake = false;
    };
    recht = {
      url = "github:kmein/recht";
      flake = false;
    };
    retiolum = {
      url = "github:krebs/retiolum";
      flake = false;
    };
    scripts = {
      url = "github:kmein/scripts";
      flake = false;
    };
    stockholm = {
      url = "git+https://cgit.lassul.us/stockholm";
      flake = false;
    };
    telebots = {
      url = "github:kmein/telebots";
      flake = false;
    };
    tinc-graph = {
      url = "github:kmein/tinc-graph";
      flake = false;
    };
    traadfri = {
      url = "github:kmein/traadfri";
      flake = false;
    };
    tuna = {
      url = "github:kmein/tuna";
      flake = false;
    };
  };

  outputs = {
    self,
    flake-utils,
    home-manager,
    krops,
    menstruation-backend,
    menstruation-telegram,
    nix-writers,
    nixpkgs,
    nixpkgs-unstable,
    recht,
    retiolum,
    scripts,
    stockholm,
    telebots,
    tinc-graph,
    traadfri,
    tuna,
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    source = name:
      {
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
      }
      // nixpkgs.lib.mapAttrs' (name: value: {
        inherit name;
        value.file = toString value;
      }) (nixpkgs.lib.filterAttrs (name: _: !builtins.elem name ["flake-utils" "krops" "self"]) inputs);
    deployScriptFor = {
      name,
      host,
    }: let
      inherit (import ./lib/default.nix) sshPort;
    in
      toString (krops.packages.${system}.writeDeploy "deploy-${name}" {
        source = krops.lib.evalSource [(source name)];
        target = "root@${host}:${toString sshPort}";
      });
  in {
    apps.${system} = let
      forSystems = f: builtins.listToAttrs (map f (builtins.attrNames (builtins.readDir ./systems)));
      deployScripts = forSystems (name: {
        name = "deploy-${name}";
        value = {
          type = "app";
          program = deployScriptFor {
            inherit name;
            host =
              if name != "ful"
              then "${name}.r"
              else "130.61.209.15";
          };
        };
      });
      ciScripts = forSystems (name: {
        name = "build-${name}";
        value = {
          type = "app";
          program = import ./ci.nix {inherit name system inputs;};
        };
      });
    in
      deployScripts // ciScripts;
  };
}
