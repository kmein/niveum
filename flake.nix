{
  description = "niveum: packages, modules, systems";

  inputs = {
    self.submodules = true;

    agenix.url = "github:ryantm/agenix";
    autorenkalender.url = "github:kmein/autorenkalender";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nixpkgs-old.url = "github:NixOS/nixpkgs/50fc86b75d2744e1ab3837ef74b53f103a9b55a0";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    retiolum.url = "github:krebs/retiolum";
    scripts.url = "github:kmein/scripts";
    stockholm.url = "github:krebs/stockholm";
    stylix.url = "github:danth/stylix/release-25.11";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    voidrice.url = "github:Lukesmithxyz/voidrice";
    wallpapers.url = "github:kmein/wallpapers";
    nix-topology.url = "github:oddlama/nix-topology";

    voidrice.flake = false;
    wallpapers.flake = false;

    naersk.url = "github:nix-community/naersk";
    fenix.url = "github:nix-community/fenix";
    naersk.inputs.fenix.follows = "fenix";
    menstruation-backend.inputs.fenix.follows = "fenix";
    tinc-graph.inputs.fenix.follows = "fenix";
    scripts.inputs.fenix.follows = "fenix";
    tinc-graph.inputs.naersk.follows = "naersk";
    scripts.inputs.naersk.follows = "naersk";

    menstruation-telegram.inputs.menstruation-backend.follows = "menstruation-backend";

    menstruation-telegram.inputs.nixpkgs.follows = "nixpkgs-old";
    telebots.inputs.nixpkgs.follows = "nixpkgs-old";

    agenix.inputs.home-manager.follows = "home-manager";

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    autorenkalender.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    nix-topology.inputs.nixpkgs.follows = "nixpkgs";
    stockholm.inputs.nixpkgs.follows = "nixpkgs";
    menstruation-backend.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    scripts.inputs.nixpkgs.follows = "nixpkgs";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      nur,
      home-manager,
      agenix,
      retiolum,
      menstruation-backend,
      menstruation-telegram,
      scripts,
      tinc-graph,
      nix-topology,
      nixos-hardware,
      treefmt-nix,
      autorenkalender,
      telebots,
      stockholm,
      nix-index-database,
      stylix,
      voidrice,
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

      apps =
        let
          localSystem = "x86_64-linux";
        in
        {
          ${localSystem} =
            let
              pkgs = import nixpkgs {
                system = localSystem;
                overlays = [ self.overlays.default ];
              };
              lib = nixpkgs.lib;
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
                    machines = import lib/machines.nix;
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

                      ${pkgs.nixos-rebuild-ng}/bin/nixos-rebuild-ng switch \
                        --max-jobs 2 \
                        --log-format internal-json \
                        --flake .#${hostname} \
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
            ];
        };

      # TODO overlay for packages
      # TODO remove flake-utils dependency from my own repos

      nixosModules = {
        moodle-dl = import modules/moodle-dl.nix;
        passport = import modules/passport.nix;
        panoptikon = import modules/panoptikon.nix;
        power-action = import modules/power-action.nix;
        system-dependent = import modules/system-dependent.nix;
        telegram-bot = import modules/telegram-bot.nix;
        go-webring = import modules/go-webring.nix;
      };

      overlays.default = final: prev: {
        niveum-terminal = prev.alacritty;
        niveum-browser = prev.firefox;
        niveum-filemanager = prev.pcmanfm;

        # wrapped from upstream
        wrapScript =
          {
            packages ? [ ],
            name,
            script,
          }:
          prev.writers.writeDashBin name ''PATH=$PATH:${
            nixpkgs.lib.makeBinPath (
              packages
              ++ [
                final.findutils
                final.coreutils
                final.gnused
                final.gnugrep
              ]
            )
          } ${script} "$@"'';
        tag = final.wrapScript {
          script = voidrice.outPath + "/.local/bin/tag";
          name = "tag";
          packages = [ final.ffmpeg ];
        };
        booksplit = final.wrapScript {
          script = voidrice.outPath + "/.local/bin/booksplit";
          name = "booksplit";
          packages = [
            final.ffmpeg
            final.glibc.bin
          ];
        };
        auc = prev.callPackage packages/auc.nix { };
        cheat-sh = prev.callPackage packages/cheat-sh.nix { };
        brassica = prev.callPackage packages/brassica.nix { }; # TODO upstream
        dawn-editor = prev.callPackage packages/dawn.nix {};
        text2pdf = prev.callPackage packages/text2pdf.nix { }; # TODO upstream
        wttr = prev.callPackage packages/wttr.nix { }; # TODO upstream
        jsesh = prev.callPackage packages/jsesh.nix { }; # TODO upstream
        opustags = prev.callPackage packages/opustags.nix { }; # TODO upstream
        trans = prev.callPackage packages/trans.nix { }; # TODO upstream
        go-webring = prev.callPackage packages/go-webring.nix { }; # TODO upstream
        stag = prev.callPackage packages/stag.nix { }; # TODO upstream
        mpv = prev.mpv.override {
          scripts = [
            final.mpvScripts.visualizer
            final.mpvScripts.mpris
          ];
        };
        morris = prev.callPackage packages/morris.nix { };
        cro = prev.callPackage packages/cro.nix { };
        dmenu = prev.writers.writeDashBin "dmenu" ''exec ${final.rofi}/bin/rofi -dmenu "$@"'';
        weechatScripts = prev.weechatScripts // {
          hotlist2extern = prev.callPackage packages/weechatScripts/hotlist2extern.nix { }; # TODO upstream
        };
        vimPlugins = prev.vimPlugins // {
          cheat-sh = prev.callPackage packages/vimPlugins/cheat-sh.nix { };
          icalendar-vim = prev.callPackage packages/vimPlugins/icalendar-vim.nix { }; # TODO upstream
          jq-vim = prev.callPackage packages/vimPlugins/jq-vim.nix { }; # TODO upstream
          typst-vim = prev.callPackage packages/vimPlugins/typst-vim.nix { }; # TODO upstream
          mdwa-nvim = prev.callPackage packages/vimPlugins/mdwa-nvim.nix { }; # TODO upstream
          vim-ernest = prev.callPackage packages/vimPlugins/vim-ernest.nix { }; # TODO upstream
          vim-256noir = prev.callPackage packages/vimPlugins/vim-256noir.nix { }; # TODO upstream
          vim-colors-paramount = prev.callPackage packages/vimPlugins/vim-colors-paramount.nix { }; # TODO upstream
          vim-fetch = prev.callPackage packages/vimPlugins/vim-fetch.nix { }; # TODO upstream
          vim-fsharp = prev.callPackage packages/vimPlugins/vim-fsharp.nix { }; # TODO upstream
          vim-mail = prev.callPackage packages/vimPlugins/vim-mail.nix { }; # TODO upstream
          vim-reason-plus = prev.callPackage packages/vimPlugins/vim-reason-plus.nix { }; # TODO upstream
        };

        # packaged from inputs
        agenix = agenix.packages.${prev.stdenv.hostPlatform.system}.default;
        pun-sort-api = scripts.packages.${prev.stdenv.hostPlatform.system}.pun-sort-api;
        alarm = scripts.packages.${prev.stdenv.hostPlatform.system}.alarm;
        menstruation-telegram =
          menstruation-telegram.packages.${prev.stdenv.hostPlatform.system}.menstruation-telegram;
        menstruation-backend =
          menstruation-backend.packages.${prev.stdenv.hostPlatform.system}.menstruation-backend;
        telebots = telebots.packages.${prev.stdenv.hostPlatform.system}.telebots;
        hesychius = scripts.packages.${prev.stdenv.hostPlatform.system}.hesychius;
        autorenkalender = autorenkalender.packages.${prev.stdenv.hostPlatform.system}.default;
        onomap = scripts.packages.${prev.stdenv.hostPlatform.system}.onomap;
        tinc-graph = tinc-graph.packages.${prev.stdenv.hostPlatform.system}.tinc-graph;

        # krebs
        brainmelter = prev.callPackage packages/brainmelter.nix { };
        cyberlocker-tools = prev.callPackage packages/cyberlocker-tools.nix { };
        hc = prev.callPackage packages/hc.nix { };
        pls = prev.callPackage packages/pls.nix { };
        radio-news = prev.callPackage packages/radio-news { };
        untilport = prev.callPackage packages/untilport.nix { };
        weechat-declarative = prev.callPackage packages/weechat-declarative.nix { };

        # my packages
        betacode = prev.callPackage packages/betacode.nix { };
        bring-out-the-gimp = prev.callPackage packages/gimp.nix { };
        closest = prev.callPackage packages/closest { };
        default-gateway = prev.callPackage packages/default-gateway.nix { };
        depp = prev.callPackage packages/depp.nix { };
        devanagari = prev.callPackage packages/devanagari { };
        radioStreams = prev.callPackage packages/streams { };
        devour = prev.callPackage packages/devour.nix { };
        dmenu-randr = prev.callPackage packages/dmenu-randr.nix { };
        emailmenu = prev.callPackage packages/emailmenu.nix { };
        fkill = prev.callPackage packages/fkill.nix { };
        fzfmenu = prev.callPackage packages/fzfmenu.nix { };
        gfs-fonts = prev.callPackage packages/gfs-fonts.nix { };
        heuretes = prev.callPackage packages/heuretes.nix { };
        image-convert-favicon = prev.callPackage packages/image-convert-favicon.nix { };
        image-convert-tolino = prev.callPackage packages/image-convert-tolino.nix { };
        ipa = prev.writers.writePython3Bin "ipa" { flakeIgnore = [ "E501" ]; } packages/ipa.py;
        kirciuoklis = prev.callPackage packages/kirciuoklis.nix { };
        kpaste = prev.callPackage packages/kpaste.nix { };
        literature-quote = prev.callPackage packages/literature-quote.nix { };
        man-pdf = prev.callPackage packages/man-pdf.nix { };
        mansplain = prev.callPackage packages/mansplain.nix { };
        manual-sort = prev.callPackage packages/manual-sort.nix { };
        mpv-iptv = prev.callPackage packages/mpv-iptv.nix { };
        mpv-radio = prev.callPackage packages/mpv-radio.nix { di-fm-key-file = "/dev/null"; };
        mpv-tuner = prev.callPackage packages/mpv-tuner.nix { di-fm-key-file = "/dev/null"; };
        mpv-tv = prev.callPackage packages/mpv-tv.nix { };
        new-mac = prev.callPackage packages/new-mac.nix { };
        nix-git = prev.callPackage packages/nix-git.nix { };
        noise-waves = prev.callPackage packages/noise-waves.nix { };
        notemenu = prev.callPackage packages/notemenu.nix { };
        obsidian-vim = prev.callPackage packages/obsidian-vim.nix { };
        vim-typewriter = prev.callPackage packages/vim-typewriter.nix { };
        polyglot = prev.callPackage packages/polyglot.nix { };
        q = prev.callPackage packages/q.nix { };
        qrpaste = prev.callPackage packages/qrpaste.nix { };
        random-zeno = prev.callPackage packages/random-zeno.nix { };
        scanned = prev.callPackage packages/scanned.nix { };
        stardict-tools = prev.callPackage packages/stardict-tools.nix { };
        swallow = prev.callPackage packages/swallow.nix { };
        tocharian-font = prev.callPackage packages/tocharian-font.nix { };
        ttspaste = prev.callPackage packages/ttspaste.nix { };
        niveum-ssh = prev.callPackage packages/niveum-ssh.nix { };
        try-connect = prev.callPackage packages/try-connect.nix { };
        unicodmenu = prev.callPackage packages/unicodmenu.nix { };
        vg = prev.callPackage packages/vg.nix { };
        vim-kmein = prev.callPackage packages/vim-kmein { };
        vimv = prev.callPackage packages/vimv.nix { };
        klem = prev.callPackage packages/klem.nix { };

        lib = lib // {
          niveum = import lib/default.nix {
            inherit lib;
            pkgs = final;
          };
          panoptikon = import lib/panoptikon.nix {
            inherit lib;
            pkgs = final;
          };
        };
      };

      nixosConfigurations =
        let
          profiles.default = [
            { nix.nixPath = [ "nixpkgs=${nixpkgs}" ]; }
            { nixpkgs.overlays = [ self.overlays.default ]; }
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            nix-topology.nixosModules.default
            configs/mycelium.nix
            configs/tor.nix
            configs/retiolum.nix
            configs/spacetime.nix
            configs/nix.nix
            configs/sshd.nix
            configs/admin-essentials.nix
          ];
          profiles.desktop = [
            home-manager.nixosModules.home-manager
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
          ];
        in
        {
          ful = nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/ful/configuration.nix
                self.nixosModules.panoptikon
                self.nixosModules.go-webring
                stockholm.nixosModules.reaktor2
                nur.modules.nixos.default
                { nixpkgs.overlays = [ stockholm.overlays.default ]; }
              ];
          };
          zaatar = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/zaatar/configuration.nix
              ];
          };
          kibbeh = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/kibbeh/configuration.nix
              ];
          };
          makanek = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.server
              ++ [
                systems/makanek/configuration.nix
                self.nixosModules.telegram-bot
                nur.modules.nixos.default
              ];
          };
          tahina = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = profiles.default ++ [
              systems/tahina/configuration.nix
            ];
          };
          tabula = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = profiles.default ++ [
              systems/tabula/configuration.nix
            ];
          };
          manakish = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/manakish/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-x230
              ];
          };
          kabsa = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/kabsa/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-x220
              ];
          };
          fatteh = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules =
              profiles.default
              ++ profiles.desktop
              ++ [
                systems/fatteh/configuration.nix
                nixos-hardware.nixosModules.lenovo-thinkpad-t480
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
              nix-topology.overlays.default
            ];
          };
        in
        {
          topology = import nix-topology {
            inherit pkgs;
            modules = [
              { nixosConfigurations = self.nixosConfigurations; }
            ];
          };
          inherit (pkgs)
            auc
            betacode
            booksplit
            brainmelter
            brassica
            cheat-sh
            closest
            cro
            cyberlocker-tools
            dawn-editor
            default-gateway
            depp
            devanagari
            devour
            dmenu-randr
            emailmenu
            fkill
            fzfmenu
            gfs-fonts
            bring-out-the-gimp
            go-webring
            hc
            heuretes
            image-convert-favicon
            image-convert-tolino
            ipa
            jsesh
            kirciuoklis
            klem
            kpaste
            literature-quote
            man-pdf
            mansplain
            manual-sort
            morris
            mpv-iptv
            mpv-radio
            mpv-tuner
            mpv-tv
            new-mac
            niveum-ssh
            nix-git
            noise-waves
            notemenu
            obsidian-vim
            opustags
            pls
            polyglot
            q
            qrpaste
            radio-news
            random-zeno
            rfc
            scanned
            stag
            stardict-tools
            swallow
            text2pdf
            timer
            tocharian-font
            trans
            try-connect
            ttspaste
            unicodmenu
            untilport
            vg
            vim-kmein
            vim-typewriter
            vimv
            weechat-declarative
            wttr
            ;
        }
      );
    };
}
