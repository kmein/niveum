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
    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
    menstruation-telegram = {
      url = "github:kmein/menstruation-telegram";
      flake = false;
    };
    menstruation-backend = {
      url = "github:kmein/menstruation.rs";
      flake = false;
    };
  };

  outputs =
  { self
  , flake-utils
  , home-manager
  , krops
  , menstruation-backend
  , menstruation-telegram
  , nix-writers
  , nixpkgs
  , nixpkgs-mozilla
  , nixpkgs-unstable
  , retiolum
  , stockholm
  }:
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
      nixpkgs-mozilla.git = { url = "https://github.com/mozilla/nixpkgs-mozilla"; ref = nixpkgs-mozilla.rev; };
      menstruation-telegram.git = { url = "https://github.com/kmein/menstruation-telegram"; ref = menstruation-telegram.rev; };
      menstruation-backend.git = { url = "https://github.com/kmein/menstruation.rs"; ref = menstruation-backend.rev; };

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
      deployScripts = builtins.listToAttrs (map (system: {
        name = "deploy-${system}";
        value = {
          type = "app";
          program = deployScriptFor { name = system; host = "${system}.r"; };
        };
      }) (builtins.attrNames (builtins.readDir ./systems)));
    in deployScripts // {
      deploy-all = {
        type = "app";
        program = toString (pkgs.writers.writeDash "deploy-all"
          (nixpkgs.lib.concatMapStringsSep "\n" (script: script.program) (builtins.attrValues deployScripts)));
      };
    };

    nixosConfigurations = {};
    hydraJobs =
      nixpkgs.lib.mapAttrs'
        (name: config: nixpkgs.lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel)
        self.nixosConfigurations;
  };
}
