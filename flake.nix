{
  description = "niveum: packages, modules, systems";

  inputs = {
    agenix.url = "github:ryantm/agenix";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    krops.url = "github:kmein/krops";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nixinate.url = "github:matthewcroughan/nixinate";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nur.url = "github:nix-community/NUR";
    recht.url = "github:kmein/recht";
    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    krops.inputs.flake-utils.follows = "flake-utils";
    krops.inputs.nixpkgs.follows = "nixpkgs";
    menstruation-backend.inputs.flake-utils.follows = "flake-utils";
    menstruation-backend.inputs.nixpkgs.follows = "nixpkgs";
    menstruation-telegram.inputs.flake-utils.follows = "flake-utils";
    menstruation-telegram.inputs.nixpkgs.follows = "nixpkgs";
    nixinate.inputs.nixpkgs.follows = "nixpkgs";
    recht.inputs.flake-utils.follows = "flake-utils";
    recht.inputs.nixpkgs.follows = "nixpkgs";
    retiolum.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.flake-utils.follows = "flake-utils";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";

    # legacy
    scripts = {
      url = "github:kmein/scripts";
      flake = false;
    };
    traadfri = {
      url = "github:kmein/traadfri";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nur,
    home-manager,
    nixinate,
    agenix,
    retiolum,
    ...
  }: let
  in {
    apps = nixinate.nixinate.x86_64-linux self;

    nixosConfigurations = {
      ful = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          systems/ful/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
        ];
      };
      zaatar = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          {
            _module.args.nixinate = {
              host = "zaatar";
              sshUser = "root";
              buildOn = "remote";
              substituteOnTarget = true;
              hermetic = false;
            };
          }
          systems/zaatar/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
        ];
      };
      makanek = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        # for using inputs in other config files
        specialArgs = {inherit inputs;};
        modules = [
          {
            _module.args.nixinate = {
              host = "makanek";
              sshUser = "root";
              buildOn = "remote";
              substituteOnTarget = true;
              hermetic = false;
            };
          }
          systems/makanek/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
          nur.nixosModules.nur
        ];
      };
      tahina = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          systems/tahina/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
        ];
      };
      tabula = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          systems/tabula/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
        ];
      };
      manakish = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          {
            _module.args.nixinate = {
              host = "manakish";
              sshUser = "root";
              buildOn = "remote";
              substituteOnTarget = true;
              hermetic = false;
            };
          }
          systems/manakish/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
          home-manager.nixosModules.home-manager
          nur.nixosModules.nur
        ];
      };
      kabsa = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          {
            _module.args.nixinate = {
              host = "kabsa";
              sshUser = "root";
              buildOn = "remote";
              substituteOnTarget = true;
              hermetic = false;
            };
          }
          systems/kabsa/configuration.nix
          agenix.nixosModules.default
          retiolum.nixosModules.retiolum
          home-manager.nixosModules.home-manager
          nur.nixosModules.nur
        ];
      };
    };
  };
}
