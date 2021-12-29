# imported from https://github.com/pinpox/nixos/blob/bdc0d47111d57cd19512c83538a01f9f9a3fc847/flake.nix
# ref https://www.youtube.com/watch?v=mJbQ--iBc1U
{
  description = "niveum systems";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # flake-utils.url = "github:numtide/flake-utils";
    # flake-compat = {
    #   url = "github:edolstra/flake-compat";
    #   flake = false;
    # };
    stockholm = {
      url = "git+https://cgit.lassul.us/stockholm";
      flake = false;
    };
    nix-writers = {
      url = "git+https://cgit.krebsco.de/nix-writers";
      flake = false;
    };
    krops = {
      url = "git+https://cgit.krebsco.de/krops";
      flake = false;
    };
    retiolum = {
      url = "github:krebs/retiolum";
      flake = false;
    };
    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nix-writers, home-manager, stockholm, ... }@inputs:
  let
    nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;

    defaultModules = [
      { _module.args.inputs = inputs; }
      {
        imports = [
          home-manager.nixosModules.home-manager
          "${stockholm}/krebs/3modules/power-action.nix"
          "${stockholm}/krebs/3modules/fetchWallpaper.nix"
          ({ pkgs, ... }: {
            nix.nixPath = [ "nixpkgs=${pkgs.path}" ];
            nixpkgs.overlays = [
              (import "${nix-writers}/pkgs")
              (import "${stockholm}/krebs/5pkgs")
            ];
          })
          ({ pkgs, ... }: { # https://github.com/Mic92/dotfiles/blob/50826e8f247909557975f4f193ecbb4162b07310/nixos/modules/upgrade-diff.nix
            system.activationScripts.diff = ''
              ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
             '';
          })
        ];

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      }
    ];
  in {
    nixosConfigurations = {
      kabsa = nixosSystem {
        system = "x86_64-linux";
        modules = defaultModules ++ [
          systems/kabsa/configuration.nix
        ];
      };
    };
  };
}
