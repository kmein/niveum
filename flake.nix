{
  description = "niveum: packages, modules, systems";

  inputs = {
    agenix.url = "github:ryantm/agenix";
    autorenkalender.url = "github:kmein/autorenkalender";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    hyprspace.url = "github:hyprspace/hyprspace";
    hyprspace.inputs.nixpkgs.follows = "nixpkgs";
    hyprspace.inputs.flake-parts.follows = "flake-parts";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nixpkgs-old.url = "github:NixOS/nixpkgs/50fc86b75d2744e1ab3837ef74b53f103a9b55a0";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    niphas.url = "git+https://code.kmein.de/kfm/niphas";
    ephemeris-service.url = "git+https://code.kmein.de/kfm/ephemeris-service";
    panoptikon.url = "git+https://code.kmein.de/kfm/panoptikon";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # NB: deliberately NOT following nixpkgs — this flake's 25.05 package defs
    # don't build against nixpkgs 26.05 (Python builder now requires an explicit
    # `format`). Let it use its own pinned nixos-25.05 (a cached release channel);
    # the driver is a self-contained closure and doesn't touch the rest of fatteh.
    nixos-06cb-009a-fingerprint-sensor.url = "github:ahbnr/nixos-06cb-009a-fingerprint-sensor?ref=25.05";
    nur.url = "github:nix-community/NUR";
    pr-notifier.url = "git+https://code.kmein.de/kfm/pr-notifier";
    kartei-ng.url = "github:krebs/kartei-ng";
    tincr.url = "github:Mic92/tincr";
    scripts.url = "git+https://code.kmein.de/kfm/to-hen";
    stockholm.url = "github:krebs/stockholm";
    stylix.url = "github:danth/stylix/release-26.05";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    voidrice.url = "github:Lukesmithxyz/voidrice";
    wetter.url = "github:kmein/wetter";
    wrappers.url = "github:lassulus/wrappers";
    opencrow.url = "github:pinpox/opencrow";
    # no `follows` overrides: keep the upstream lock so the store paths
    # match numtide's binary cache (omp is expensive to build from source)
    llm-agents.url = "github:numtide/llm-agents.nix";

    voidrice.flake = false;

    naersk.url = "github:nix-community/naersk";
    fenix.url = "github:nix-community/fenix";
    naersk.inputs.fenix.follows = "fenix";
    menstruation-backend.inputs.fenix.follows = "fenix";
    tinc-graph.inputs.fenix.follows = "fenix";
    scripts.inputs.fenix.follows = "fenix";
    pr-notifier.inputs.naersk.follows = "naersk";
    tinc-graph.inputs.naersk.follows = "naersk";
    scripts.inputs.naersk.follows = "naersk";

    menstruation-telegram.inputs.menstruation-backend.follows = "menstruation-backend";

    menstruation-telegram.inputs.nixpkgs.follows = "nixpkgs-old";
    telebots.inputs.nixpkgs.follows = "nixpkgs-old";

    agenix.inputs.home-manager.follows = "home-manager";

    opencrow.inputs.treefmt-nix.follows = "treefmt-nix";

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.systems.follows = "systems";
    autorenkalender.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
    panoptikon.inputs.nixpkgs.follows = "nixpkgs";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    nur.inputs.flake-parts.follows = "flake-parts";
    niphas.inputs.nixpkgs.follows = "nixpkgs-unstable";
    niphas.inputs.treefmt-nix.follows = "treefmt-nix";
    niphas.inputs.wrappers.follows = "wrappers";
    pr-notifier.inputs.nixpkgs.follows = "nixpkgs";
    stockholm.inputs.nixpkgs.follows = "nixpkgs";
    kartei-ng.inputs.nixpkgs.follows = "nixpkgs";
    tincr.inputs.nixpkgs.follows = "nixpkgs";
    tincr.inputs.treefmt-nix.follows = "treefmt-nix";
    menstruation-backend.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    scripts.inputs.nixpkgs.follows = "nixpkgs";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    stylix.inputs.nur.follows = "nur";
    stylix.inputs.flake-parts.follows = "flake-parts";
    stylix.inputs.systems.follows = "systems";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";
    wetter.inputs.nixpkgs.follows = "nixpkgs";
    wrappers.inputs.nixpkgs.follows = "nixpkgs";
    opencrow.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-old,
      nur,
      hyprspace,
      home-manager,
      agenix,
      menstruation-backend,
      menstruation-telegram,
      scripts,
      tinc-graph,
      opencrow,
      llm-agents,
      nixpkgs-unstable,
      nixos-hardware,
      nixos-06cb-009a-fingerprint-sensor,
      niphas,
      ephemeris-service,
      treefmt-nix,
      pr-notifier,
      autorenkalender,
      telebots,
      stockholm,
      panoptikon,
      nix-index-database,
      stylix,
      voidrice,
      wetter,
      wrappers,
      ...
    }:
    let
      lib = nixpkgs.lib;
      eachSupportedSystem = lib.genAttrs lib.systems.flakeExposed;
      treefmtEval = eachSupportedSystem (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} (
          { pkgs, ... }:
          {
            projectRootFile = "flake.nix";
            programs.nixfmt.enable = true;
            programs.ormolu.enable = true;
            programs.black.enable = true;
            programs.prettier.enable = true;
            programs.stylua.enable = true;
          }
        )
      );
    in
    {

      apps = eachSupportedSystem (
        localSystem:
        let
          pkgs = import nixpkgs {
            system = localSystem;
            overlays = [ self.overlays.default ];
          };
          machines = import lib/machines.nix;
        in
        lib.mergeAttrsList [
          {
            mock-secrets = {
              type = "app";
              program = toString (
                pkgs.writers.writeDash "mock-secrets" ''
                  ${pkgs.findutils}/bin/find secrets -not -path '*/.*' -type f  | ${pkgs.coreutils}/bin/sort > secrets.txt
                ''
              );
            };
          }
          (builtins.listToAttrs (
            map (
              hostname:
              let
                deployScript = pkgs.writers.writeBash "deploy-${hostname}" ''
                  reachable=$(${pkgs.try-connect.${hostname}}/bin/try-connect)

                  if [ -z "$reachable" ]; then
                    exit 1
                  fi

                  target="root@$reachable"
                  echo "Deploying to ${hostname} via $target"

                  # Set SSH options based on address type
                  if [[ "$reachable" == *.onion ]]; then
                    # why? ControlPath=none
                    # SSH is trying to create a control socket with a path that includes
                    # the full .onion hostname, and Unix domain sockets have a path length
                    # limit (typically 108 characters). The .onion address is too long.
                    export NIX_SSHOPTS="-p ${
                      toString machines.${hostname}.sshPort
                    } -o ProxyCommand='${pkgs.netcat}/bin/nc -x localhost:9050 %h %p' -o ControlPath=none"
                  else
                    export NIX_SSHOPTS="-p ${toString machines.${hostname}.sshPort}"
                  fi

                  ${lib.getExe pkgs.nixos-rebuild-ng} switch \
                    --max-jobs 2 \
                    --log-format internal-json \
                    --flake .?submodules=1#${hostname} \
                    --use-substitutes \
                    --target-host "$target" \
                    ${lib.optionalString (localSystem != machines.${hostname}.system) "--build-host $target"} \
                    |& ${pkgs.nix-output-monitor}/bin/nom --json
                '';
              in
              lib.attrsets.nameValuePair "deploy-${hostname}" {
                type = "app";
                program = toString deployScript;
              }
            ) (builtins.attrNames self.nixosConfigurations)
          ))
        ]
      );

      # TODO overlay for packages
      # TODO remove flake-utils dependency from my own repos

      nixosModules = {
        moodle-dl = import modules/moodle-dl.nix;
        passport = import modules/passport.nix;
        power-action = import modules/power-action.nix;
        system-dependent = import modules/system-dependent.nix;
        telegram-bot = import modules/telegram-bot.nix;
        go-webring = import modules/go-webring.nix;
      };

      overlays.gpod-utils = final: prev: {
        gpod-utils = prev.callPackage packages/gpod-utils { };
      };

      overlays.default = lib.composeManyExtensions [
        (import overlays/packages.nix)
        (import overlays/unstable.nix { inherit nixpkgs-unstable; })
        (import overlays/wrappers.nix { inherit voidrice; })
        (import overlays/inputs.nix {
          inherit
            opencrow
            llm-agents
            wetter
            agenix
            scripts
            menstruation-telegram
            menstruation-backend
            telebots
            autorenkalender
            tinc-graph
            ephemeris-service
            ;
        })
        (import overlays/lib.nix)
      ];

      nixosConfigurations =
        let
          profiles.minimal = [
            { nix.nixPath = [ "nixpkgs=${nixpkgs}" ]; }
            {
              nixpkgs.overlays = [
                self.overlays.default
                pr-notifier.overlays.default
                panoptikon.overlays.default
              ];
            }
            {
              system.autoUpgrade = {
                enable = true;
                flake = self.outPath;
                flags = [
                  "--print-build-logs"
                ];
                dates = "02:00";
                randomizedDelaySec = "45min";
              };
            }
            agenix.nixosModules.default
            niphas.nixosModules.nix
            niphas.nixosModules.shell
            configs/spacetime.nix
            configs/sshd.nix
            configs/tor.nix
          ];
          profiles.default = profiles.minimal ++ [
            hyprspace.nixosModules.default
            modules/retiolum.nix
            configs/retiolum.nix
            configs/hyprspace.nix
          ];
          profiles.desktop = [
            niphas.nixosModules.editor
            niphas.nixosModules.git
            niphas.nixosModules.udiskie
            niphas.nixosModules.desktop
            configs/niphas.nix
            home-manager.nixosModules.home-manager
            {
              # share the system pkgs (incl. overlays — otherwise HM evaluates
              # its own nixpkgs without them, yielding e.g. a stale niri) and
              # install user packages to /etc/profiles/per-user instead of
              # mutating ~/.nix-profile imperatively on activation
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
            nix-index-database.nixosModules.default
            nur.modules.nixos.default
            stylix.nixosModules.stylix
            self.nixosModules.system-dependent
            self.nixosModules.power-action
          ];
          profiles.server = [
            configs/save-space.nix
            configs/monitoring.nix
            self.nixosModules.passport
          ];
        in
        {
          ful = nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/ful/configuration.nix
                panoptikon.nixosModules.default
                self.nixosModules.go-webring
                pr-notifier.nixosModules.default
                stockholm.nixosModules.reaktor2
                opencrow.nixosModules.default
                nur.modules.nixos.default
                {
                  nixpkgs.overlays = [
                    stockholm.overlays.default
                  ];
                }
              ];
          };
          zaatar = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/zaatar/configuration.nix
              ];
          };
          kibbeh = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/kibbeh/configuration.nix
              ];
          };
          makanek = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/makanek/configuration.nix
                self.nixosModules.telegram-bot
                nur.modules.nixos.default
              ];
          };
          khall = nixpkgs.lib.nixosSystem {
            # nix build .#nixosConfigurations.khall.config.system.build.sdImage
            # zstdcat result/sd-image/nixos-image-*.img.zst | sudo dd of=/dev/sdX bs=4M conv=fsync status=progress
            system = "aarch64-linux";
            specialArgs = { inherit self; };
            modules = profiles.minimal ++ [
              /*
                {
                  nixpkgs = {
                    buildPlatform = "x86_64-linux";
                    hostPlatform = "aarch64-linux";
                  };
                }
              */
              (
                { pkgs, ... }:
                {
                  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_rpi3;
                  boot.supportedFilesystems = lib.mkForce [
                    "ext4"
                    "vfat"
                    "tmpfs"
                  ];
                  boot.initrd.supportedFilesystems = lib.mkForce [
                    "ext4"
                    "vfat"
                  ];
                }
              )
              nixos-hardware.nixosModules.raspberry-pi-3
              hyprspace.nixosModules.default
              "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
              configs/save-space.nix
              configs/hyprspace.nix
              systems/khall/configuration.nix
            ];
          };
          tahina = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules = profiles.default ++ [
              systems/tahina/configuration.nix
            ];
          };
          tabula = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules = [
              {
                nixpkgs.overlays = [ self.overlays.default ];
              }
              systems/tabula/configuration.nix
            ];
          };
          manakish = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/manakish/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-x220
              ];
          };
          kabsa = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/kabsa/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-x230
              ];
          };
          fatteh = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit self; };
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/fatteh/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-t480
                nixos-06cb-009a-fingerprint-sensor.nixosModules."06cb-009a-fingerprint-sensor"
              ];
          };
        };

      formatter = eachSupportedSystem (system: treefmtEval.${system}.config.build.wrapper);
      checks = eachSupportedSystem (system: {
        formatting = treefmtEval.${system}.config.build.check self;
      });

      packages = eachSupportedSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              nur.overlays.default
              self.overlays.default
            ];
          };
          oldPkgs = import nixpkgs-old {
            inherit system;
            overlays = [ self.overlays.gpod-utils ];
            config = { };
          };
          # radioStreams is exported as `streams` (the TSV) below
          localNames = builtins.attrNames (
            builtins.removeAttrs (import packages/default.nix pkgs) [ "radioStreams" ]
          );
        in
        lib.getAttrs localNames pkgs
        // {
          inherit (oldPkgs) gpod-utils;
          streams = pkgs.radioStreams.tsv;
          # not in packages/: booksplit wraps a voidrice script,
          # rfc and timer are re-exported from nixpkgs
          inherit (pkgs) booksplit rfc timer;
        }
      );
    };

  nixConfig = {
    extra-substituters = [ "https://cache.numtide.com" ];
    extra-trusted-public-keys = [ "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=" ];
  };
}
