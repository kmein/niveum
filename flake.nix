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
    ensureFiles = paths: pkgs.runCommand "directory" {} ''
      set -efu
      mkdir $out
      cd $out
      ${nixpkgs.lib.concatMapStringsSep "\n" (path: ''
        mkdir -p "$(dirname ${nixpkgs.lib.escapeShellArg path})"
        echo foo > ${nixpkgs.lib.escapeShellArg path}
      '') paths}
    '';
  in {
    apps.${system} = let
      forSystems = f: builtins.listToAttrs (map f (builtins.attrNames (builtins.readDir ./systems)));
      deployScripts = forSystems (system: {
        name = "deploy-${system}";
        value = {
          type = "app";
          program = deployScriptFor { name = system; host = "${system}.r"; };
        };
      });
      nixPathFor = system: nixpkgs.lib.concatStringsSep ":" [
        "nixos-config=${toString ./.}/systems/${system}/configuration.nix"
        "niveum=${toString ./.}"
        "nixpkgs=${nixpkgs}"
        "nixpkgs-unstable=${nixpkgs-unstable}"
        "stockholm=${stockholm}"
        "home-manager=${home-manager}"
        "nix-writers=${nix-writers}"
        "retiolum=${retiolum}"
        "system-secrets=${systemSecrets.${system}}"
        "secrets=${sharedSecrets}"
        "menstruation-backend=${menstruation-backend}"
        "menstruation-telegram=${menstruation-telegram}"
      ];
      # cd ~/.password-store/shared && find * -type f | sed 's/.gpg$//'
      sharedSecrets = ensureFiles [
        "di.fm/key"
        "eduroam/identity"
        "eduroam/password"
        "fritznas.smb"
        "mail/cock"
        "mail/fastmail"
        "mail/gmail/amroplay"
        "mail/gmail/kieran.meinhardt"
        "mail/meinhaki"
        "mail/meinhaki.cert"
        "mail/posteo"
        "nextcloud-fysi/password"
        "nextcloud/password"
        "openweathermap.key"
        "posteo/password"
        "spotify/password"
        "spotify/username"
        "traadfri.key"
        "wifi/Aether.psk"
      ];
      systemSecrets = let basic = [ "retiolum.ed25519" "retiolum.key" "syncthing/cert.pem" "syncthing/key.pem"]; in {
        zaatar = ensureFiles ([ "moodle.token" "telegram/moodle-dl.token" ] ++ basic);
        kabsa = ensureFiles basic;
        manakish = ensureFiles basic;
        makanek = ensureFiles ([
          "irc/retiolum"
          "irc/hackint"
          "irc/libera"
          "irc/oftc"
          "matrix/nibbana"
          "maxmind/license.key"
          "moodle-dl/faye.token"
          "nextcloud/admin"
          "nextcloud/database"
          "telegram/nachtischsatan.token"
          "telegram/reverse.token"
          "telegram/odyssey.token"
          "telegram/betacode.token"
          "telegram/moodle-dl.token"
          "telegram/proverb.token"
          "telegram/menstruation.token"
          "telegram/cool_village.token"
          "telegram/kmein.token"
          "telegram/prometheus.token"
          "weechat/relay"
        ] ++ basic);
      };
      ciScripts = forSystems (system: {
        name = "build-${system}";
        value = {
          type = "app";
          program = toString (pkgs.writers.writeDash "build" "NIX_PATH=${nixPathFor system} nix-build '<nixpkgs/nixos>' -A system --dry-run");
        };
      });
    in deployScripts // ciScripts // {
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
