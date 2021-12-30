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
  };

  outputs =
  { self
  , flake-utils
  , home-manager
  , krops
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
      niveum-status = {
        type = "app";
        program = let
          statusCommand = pkgs.writers.writeDash "niveum-status-one" ''
            [ $# -eq 1 ] || {
              echo "Please provide a niveum system hostname." >&2
              exit 1
            }

            hostname="$1"
            version_file=/etc/niveum/version

            if commit_id="$(${pkgs.coreutils}/bin/timeout 2s ${pkgs.openssh}/bin/ssh "$hostname" cat $version_file 2>/dev/null)"; then
              ${pkgs.git}/bin/git log -1 --oneline "$commit_id"
            else
              echo offline
            fi
          '';
        in toString (pkgs.writers.writeDash "niveum-status" ''
          if [ $# -gt 0 ]; then
            systems="$@"
          else
            systems="$(ls ${toString ./.}/systems)"
          fi
          ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' -q ${statusCommand} '{1}' ::: $systems
        '');
      };
    };

    nixosConfigurations = {};
    hydraJobs =
      nixpkgs.lib.mapAttrs'
        (name: config: nixpkgs.lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel)
        self.nixosConfigurations;
  };
}
