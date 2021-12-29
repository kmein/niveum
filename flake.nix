{
  description = "niveum systems";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    secrets = {
      url = "/home/kfm/.password-store";
      flake = false;
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    retiolum = {
      url = "github:krebs/retiolum";
      flake = false;
    };
    nix-writers = {
      url = "git+https://cgit.krebsco.de/nix-writers";
      flake = false;
    };
    stockholm = {
      url = "git+https://cgit.krebsco.de/stockholm";
      flake = false;
    };
    krops = {
      url = "github:Mic92/krops";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, stockholm, secrets, nixpkgs-unstable, home-manager, retiolum, krops, nix-writers, ... }@inputs:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    writeCommand = krops.packages.${system}.writeCommand;
    niveumSystem = configuration: nixpkgs.lib.nixosSystem {
      inherit system;
      modules = defaultModules ++ [ configuration ];
    };
    defaultModules = [
      { _module.args.inputs = inputs; }
      ({ pkgs, ... }: {
        nix = {
          nixPath = [ "nixpkgs=${pkgs.path}" ];
          package = pkgs.nixFlakes;
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };
        nixpkgs.overlays = [
          (_self: _super: {
            unstable = nixpkgs-unstable.legacyPackages.${pkgs.system};
          })
          (import "${nix-writers}/pkgs")
          # (import <stockholm/krebs/5pkgs>)
        ];
      })
      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      }
    ];
  in
  {
    nixosConfigurations = {
      kabsa = niveumSystem systems/kabsa/configuration.nix;
      makanek = niveumSystem systems/makanek/configuration.nix;
      manakish = niveumSystem systems/manakish/configuration.nix;
      zaatar = niveumSystem systems/zaatar/configuration.nix;
    };

    apps.${system} = {
      #  nix run ".#deploy.kabsa"
      deploy = pkgs.callPackage ./deploy.nix {
        inherit secrets writeCommand;
        inherit (krops) lib;
      };

      #  nix run ".#test.kabsa"
      test = pkgs.callPackage ./deploy.nix {
        inherit secrets writeCommand;
        inherit (krops) lib;
        nixosRebuildCommand = "test";
      };
    };
  };
}
