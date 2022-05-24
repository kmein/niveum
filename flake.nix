{
  description = "niveum: packages, modules, systems";

  inputs = {
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-21.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    krops = {
      url = "github:Mic92/krops";
      inputs.nixpkgs.follows = "nixos-stable";
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
    nixos-unstable,
    nixos-stable,
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
    pkgs = nixos-stable.legacyPackages.${system};
    source = {
      sources,
      unstable,
      name,
    }:
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
        nixpkgs.file = toString (
          if unstable
          then inputs.nixos-unstable
          else inputs.nixos-stable
        );
      }
      // nixos-stable.lib.mapAttrs' (name: value: {
        inherit name;
        value.file = toString value;
      }) (nixos-stable.lib.filterAttrs (name: _: builtins.elem name sources) inputs);
    deployScriptFor = {
      name,
      user ? "root",
      host,
      unstable ? false,
      sshPort ? (import ./lib/default.nix).sshPort,
      sources,
    }:
      toString (krops.packages.${system}.writeDeploy "deploy-${name}" {
        source = krops.lib.evalSource [(source {inherit sources unstable name;})];
        target = "${user}@${host}:${toString sshPort}";
      });
  in {
    apps.${system} = let
      forSystems = f: builtins.listToAttrs (map f (builtins.attrNames (builtins.readDir ./systems)));
      externalNetwork = import ./lib/external-network.nix;
      deployScripts = forSystems (name: {
        name = "deploy-${name}";
        value = {
          type = "app";
          program = deployScriptFor {
            inherit name;
            host =
              if externalNetwork ? name
              then externalNetwork.${name}
              else "${name}.r";
            unstable = name == "kabsa" || name == "manakish";
            sources =
              ["nix-writers" "nixpkgs" "retiolum"]
              ++ {
                zaatar = ["traadfri"];
                ful = [];
                kabsa = ["traadfri" "nixos-unstable" "home-manager" "menstruation-backend" "recht"];
                manakish = ["traadfri" "nixos-unstable" "home-manager" "menstruation-backend" "recht"];
                makanek = ["nixos-unstable" "menstruation-telegram" "menstruation-backend" "scripts" "telebots" "tinc-graph"];
              }
              .${name};
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
