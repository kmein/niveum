{
  description = "niveum: packages, modules, systems";

  inputs = {
    agenix.url = "github:ryantm/agenix";
    # alew-web.url = "git+ssh://gitea@code.kmein.de:22022/kfm/alew-web.git?ref=refs/heads/master";
    coptic-dictionary.url = "github:kmein/coptic-dictionary";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nix-on-droid.url = "github:t184256/nix-on-droid/release-23.05";
    nixinate.url = "github:matthewcroughan/nixinate";
    nixpkgs-old.url = "github:NixOS/nixpkgs/50fc86b75d2744e1ab3837ef74b53f103a9b55a0";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nur.url = "github:nix-community/NUR";
    recht.url = "github:kmein/recht";
    retiolum.url = "github:krebs/retiolum";
    rust-overlay.url = "github:oxalica/rust-overlay";
    scripts.url = "github:kmein/scripts";
    stockholm.url = "github:krebs/stockholm";
    stylix.url = "github:danth/stylix/release-24.11";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";
    voidrice.url = "github:Lukesmithxyz/voidrice";
    wallpaper-generator.url = "github:pinpox/wallpaper-generator/v1.1";
    wallpapers.url = "github:kmein/wallpapers";

    agenix.inputs.home-manager.follows = "home-manager";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    coptic-dictionary.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # menstruation-backend.inputs.flake-utils.follows = "flake-utils";
    # menstruation-backend.inputs.nixpkgs.follows = "nixpkgs";
    # menstruation-backend.inputs.rust-overlay.follows = "rust-overlay";
    menstruation-telegram.inputs.flake-utils.follows = "flake-utils";
    menstruation-telegram.inputs.menstruation-backend.follows = "menstruation-backend";
    menstruation-telegram.inputs.nixpkgs.follows = "nixpkgs-old";
    nix-on-droid.inputs.home-manager.follows = "home-manager";
    nix-on-droid.inputs.nixpkgs.follows = "nixpkgs";
    recht.inputs.flake-utils.follows = "flake-utils";
    recht.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    scripts.inputs.flake-utils.follows = "flake-utils";
    scripts.inputs.nixpkgs.follows = "nixpkgs";
    scripts.inputs.rust-overlay.follows = "rust-overlay";
    stylix.inputs.home-manager.follows = "home-manager";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.flake-utils.follows = "flake-utils";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.rust-overlay.follows = "rust-overlay";
    voidrice.flake = false;
    wallpaper-generator.inputs.flake-utils.follows = "flake-utils";
    wallpapers.flake = false;
  };

  nixConfig = {
    extra-substituters = [ "https://kmein.cachix.org" ];
    extra-trusted-public-keys = [ "kmein.cachix.org-1:rsJ2b6++VQHJ1W6rGuDUYsK/qUkFA3bNpO6PyEyJ9Ls=" ];
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    home-manager,
    agenix,
    retiolum,
    nixinate,
    flake-utils,
    nix-on-droid,
    stylix,
    ...
  }:
    {
      apps = {
        x86_64-darwin = let
          pkgs = nixpkgs.legacyPackages.x86_64-darwin;
        in {
          deploy-maakaron = {
            type = "app";
            program = toString (pkgs.writers.writeDash "deploy-maakaron" ''
              exec $(nix build .#homeConfigurations.maakaron.activationPackage --no-link --print-out-paths)/activate
            '');
          };
        };
        x86_64-linux = let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          lib = nixpkgs.lib;
        in
          nixinate.nixinate.x86_64-linux self
          // {
            mock-secrets = {
              type = "app";
              program = toString (pkgs.writers.writeDash "mock-secrets" ''
                ${pkgs.findutils}/bin/find secrets -not -path '*/.*' -type f  | ${pkgs.coreutils}/bin/sort > secrets.txt
              '');
            };
          }
          # the following error prevents remote building of ful: https://github.com/NixOS/nixpkgs/issues/177873
          // builtins.listToAttrs (map (hostname: let
            externalNetwork = import ./lib/external-network.nix;
            targets = {
              ful = "root@ful";
              zaatar = "root@zaatar";
              makanek = "root@makanek";
              manakish = "root@manakish";
              tahina = "root@tahina";
              tabula = "root@tabula";
              kabsa = "root@kabsa";
              fatteh = "root@fatteh";
              kibbeh = "root@kibbeh";
            };
          in
            lib.attrsets.nameValuePair "deploy-${hostname}" {
              type = "app";
              program = toString (pkgs.writers.writeDash "deploy-${hostname}" ''
                exec ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch \
                  --max-jobs 2 \
                  --log-format internal-json \
                  --flake .?submodules=1#${hostname} \
                  --target-host ${targets.${hostname}} 2>&1 \
                  | ${pkgs.nix-output-monitor}/bin/nom --json
              '');
            }) (builtins.attrNames self.nixosConfigurations))
          // {
            deploy-ful = {
              type = "app";
              program = toString (pkgs.writers.writeDash "deploy-ful" ''
                exec ${pkgs.nix}/bin/nix run .?submodules=1#nixinate.ful \
                  --log-format internal-json 2>&1 \
                  | ${pkgs.nix-output-monitor}/bin/nom --json
              '');
            };
          };
      };

      nixosModules = {
        htgen = import modules/htgen.nix;
        moodle-dl = import modules/moodle-dl.nix;
        networkmanager-declarative = import modules/networkmanager-declarative.nix;
        passport = import modules/passport.nix;
        panoptikon = import modules/panoptikon.nix;
        power-action = import modules/power-action.nix;
        system-dependent = import modules/system-dependent.nix;
        telegram-bot = import modules/telegram-bot.nix;
      };

      lib = {
        panoptikon = import lib/panoptikon.nix;
      };

      nixOnDroidConfigurations = {
        moto = nix-on-droid.lib.nixOnDroidConfiguration {
          modules = [systems/moto/configuration.nix];
          pkgs = import nixpkgs {
            system = "aarch64-linux";
            overlays = [nix-on-droid.overlays.default];
          };
          extraSpecialArgs = {
            niveumPackages = inputs.self.packages.aarch64-linux;
            niveumLib = inputs.self.lib;
            inherit inputs;
          };
          home-manager-path = home-manager.outPath;
        };
      };

      homeConfigurations = {
        maakaron = let
          system = "x86_64-darwin";
          pkgs = nixpkgs.legacyPackages.${system};
        in
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [./systems/maakaron/home.nix];
            extraSpecialArgs = {
              inherit inputs;
              niveumPackages = inputs.self.packages.${system};
            };
          };
      };

      nixosConfigurations = let
        niveumSpecialArgs = system: {
          unstablePackages = import nixpkgs-unstable {
            inherit system;
            config.allowUnfreePredicate = pkg:
              builtins.elem (nixpkgs-unstable.lib.getName pkg) [
                "obsidian"
                "zoom"
              ];
          };

          niveumPackages = inputs.self.packages.${system};
          niveumLib = inputs.self.lib;
          inherit inputs;
        };
      in {
        ful = nixpkgs.lib.nixosSystem rec {
          system = "aarch64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/ful/configuration.nix
            agenix.nixosModules.default
            inputs.self.nixosModules.passport
            inputs.self.nixosModules.panoptikon
            inputs.self.nixosModules.htgen
            inputs.stockholm.nixosModules.reaktor2
            retiolum.nixosModules.retiolum
            nur.modules.nixos.default
            { nixpkgs.overlays = [ inputs.stockholm.overlays.default ]; }
            {
              _module.args.nixinate = {
                host = "ful";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
          ];
        };
        zaatar = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/zaatar/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        kibbeh = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/kibbeh/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        makanek = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          # for using inputs in other config files
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/makanek/configuration.nix
            inputs.self.nixosModules.telegram-bot
            inputs.self.nixosModules.htgen
            inputs.self.nixosModules.passport
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            nur.modules.nixos.default
          ];
        };
        tahina = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/tahina/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        tabula = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/tabula/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        manakish = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/manakish/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            home-manager.nixosModules.home-manager
            nur.modules.nixos.default
            stylix.nixosModules.stylix
          ];
        };
        kabsa = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/kabsa/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            home-manager.nixosModules.home-manager
            nur.modules.nixos.default
            stylix.nixosModules.stylix
          ];
        };
        fatteh = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/fatteh/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            home-manager.nixosModules.home-manager
            nur.modules.nixos.default
            stylix.nixosModules.stylix
          ];
        };
      };
    }
    // flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux flake-utils.lib.system.x86_64-darwin flake-utils.lib.system.aarch64-linux] (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          nur.overlays.default
          (self: super: {
            mpv = super.mpv.override {scripts = [inputs.self.packages.${system}.mpv-visualizer super.mpvScripts.mpris];};
            dmenu = super.writers.writeDashBin "dmenu" ''exec ${pkgs.rofi}/bin/rofi -dmenu "$@"'';
          })
        ];
      };
      unstablePackages = import nixpkgs-unstable {
        inherit system;
      };
      wrapScript = {
        packages ? [],
        name,
        script,
      }:
        pkgs.writers.writeDashBin name ''PATH=$PATH:${nixpkgs.lib.makeBinPath (packages ++ [pkgs.findutils pkgs.coreutils pkgs.gnused pkgs.gnugrep])} ${script} "$@"'';
    in {
      packages = rec {
        auc = pkgs.callPackage packages/auc.nix {};
        betacode = pkgs.callPackage packages/betacode.nix {};
        brassica = pkgs.callPackage packages/brassica.nix {};
        cheat-sh = pkgs.callPackage packages/cheat-sh.nix {};
        closest = pkgs.callPackage packages/closest {};
        cro = pkgs.callPackage packages/cro.nix {};
        cyberlocker-tools = pkgs.callPackage packages/cyberlocker-tools.nix {};
        default-gateway = pkgs.callPackage packages/default-gateway.nix {};
        depp = pkgs.callPackage packages/depp.nix {};
        dashboard = pkgs.callPackage packages/dashboard {};
        devanagari = pkgs.callPackage packages/devanagari {};
        devour = pkgs.callPackage packages/devour.nix {};
        dic = pkgs.callPackage packages/dic.nix {};
        dirmir = pkgs.callPackage packages/dirmir.nix {};
        dmenu-bluetooth = pkgs.callPackage packages/dmenu-bluetooth.nix {};
        dmenu-scrot = pkgs.callPackage packages/dmenu-scrot.nix {};
        dns-sledgehammer = pkgs.callPackage packages/dns-sledgehammer.nix {};
        fkill = pkgs.callPackage packages/fkill.nix {};
        fzfmenu = pkgs.callPackage packages/fzfmenu.nix {};
        genius = pkgs.callPackage packages/genius.nix {};
        gfs-fonts = pkgs.callPackage packages/gfs-fonts.nix {};
        git-preview = pkgs.callPackage packages/git-preview.nix {};
        gpt35 = pkgs.callPackage packages/gpt.nix {model = "gpt-3.5-turbo";};
        gpt4 = pkgs.callPackage packages/gpt.nix {model = "gpt-4";};
        hc = pkgs.callPackage packages/hc.nix {};
        jq-lsp = pkgs.callPackage packages/jq-lsp.nix {};
        stardict-tools = pkgs.callPackage packages/stardict-tools.nix {};
        heuretes = pkgs.callPackage packages/heuretes.nix {};
        htgen = pkgs.callPackage packages/htgen.nix {};
        image-convert-favicon = pkgs.callPackage packages/image-convert-favicon.nix {};
        image-convert-tolino = pkgs.callPackage packages/image-convert-tolino.nix {};
        infschmv = pkgs.callPackage packages/infschmv.nix {};
        iolanguage = pkgs.callPackage packages/iolanguage.nix {};
        ipa = pkgs.writers.writePython3Bin "ipa" {flakeIgnore = ["E501"];} (builtins.readFile packages/ipa.py);
        ix = pkgs.callPackage packages/ix.nix {};
        jsesh = pkgs.callPackage packages/jsesh.nix {};
        k-lock = pkgs.callPackage packages/k-lock.nix {};
        kirciuoklis = pkgs.callPackage packages/kirciuoklis.nix {};
        klem = pkgs.callPackage packages/klem.nix {};
        kpaste = pkgs.callPackage packages/kpaste.nix {};
        literature-quote = pkgs.callPackage packages/literature-quote.nix {};
        mahlzeit = pkgs.haskellPackages.callPackage packages/mahlzeit.nix {};
        man-pandoc = pkgs.callPackage packages/man/pandoc.nix {};
        man-pdf = pkgs.callPackage packages/man-pdf.nix {};
        mansplain = pkgs.callPackage packages/mansplain.nix {};
        manual-sort = pkgs.callPackage packages/manual-sort.nix {};
        menu-calc = pkgs.callPackage packages/menu-calc.nix {};
        meteo = pkgs.callPackage packages/meteo.nix {};
        noise-waves = pkgs.callPackage packages/noise-waves.nix {};
        mpv-radio = pkgs.callPackage packages/mpv-radio.nix {di-fm-key-file = "/dev/null";};
        mpv-tuner = pkgs.callPackage packages/mpv-tuner.nix {di-fm-key-file = "/dev/null";};
        mpv-tv = pkgs.callPackage packages/mpv-tv.nix {};
        mpv-iptv = pkgs.callPackage packages/mpv-iptv.nix {};
        mpv-visualizer = unstablePackages.mpvScripts.visualizer;
        new-mac = pkgs.callPackage packages/new-mac.nix {};
        nix-git = pkgs.callPackage packages/nix-git.nix {};
        nix-index-update = pkgs.callPackage packages/nix-index-update.nix {inherit system;};
        notemenu = pkgs.callPackage packages/notemenu.nix {niveumPackages = self.packages.${system};};
        opustags = pkgs.callPackage packages/opustags.nix {};
        pls = pkgs.callPackage packages/pls.nix {};
        polyglot = pkgs.callPackage packages/polyglot.nix {};
        qrpaste = pkgs.callPackage packages/qrpaste.nix {};
        random-zeno = pkgs.callPackage packages/random-zeno.nix {};
        rfc = pkgs.callPackage packages/rfc.nix {};
        gimp = pkgs.callPackage packages/gimp.nix {};
        scanned = pkgs.callPackage packages/scanned.nix {};
        swallow = pkgs.callPackage packages/swallow.nix {};
        text2pdf = pkgs.callPackage packages/text2pdf.nix {};
        timer = pkgs.callPackage packages/timer.nix {};
        tocharian-font = pkgs.callPackage packages/tocharian-font.nix {};
        trans = pkgs.callPackage packages/trans.nix {};
        ttspaste = pkgs.callPackage packages/ttspaste.nix {};
        unicodmenu = pkgs.callPackage packages/unicodmenu.nix {};
        emailmenu = pkgs.callPackage packages/emailmenu.nix {};
        untilport = pkgs.callPackage packages/untilport.nix {};
        vg = pkgs.callPackage packages/vg.nix {};
        vim = pkgs.callPackage packages/vim.nix {niveumPackages = self.packages.${system};};
        obsidian-vim = pkgs.callPackage packages/obsidian-vim.nix {};
        vimPlugins-cheat-sh-vim = pkgs.callPackage packages/vimPlugins/cheat-sh.nix {};
        vimPlugins-icalendar-vim = pkgs.callPackage packages/vimPlugins/icalendar-vim.nix {};
        vimPlugins-jq-vim = pkgs.callPackage packages/vimPlugins/jq-vim.nix {};
        vimPlugins-typst-vim = pkgs.callPackage packages/vimPlugins/typst-vim.nix {};
        vimPlugins-vim-256noir = pkgs.callPackage packages/vimPlugins/vim-256noir.nix {};
        vimPlugins-vim-colors-paramount = pkgs.callPackage packages/vimPlugins/vim-colors-paramount.nix {};
        vimPlugins-vim-fetch = pkgs.callPackage packages/vimPlugins/vim-fetch.nix {};
        vimPlugins-vim-fsharp = pkgs.callPackage packages/vimPlugins/vim-fsharp.nix {};
        vimPlugins-vim-mail = pkgs.callPackage packages/vimPlugins/vim-mail.nix {};
        vimPlugins-vim-reason-plus = pkgs.callPackage packages/vimPlugins/vim-reason-plus.nix {};
        vimv = pkgs.callPackage packages/vimv.nix {};
        weechat-declarative = pkgs.callPackage packages/weechat-declarative.nix {};
        weechatScripts-hotlist2extern = pkgs.callPackage packages/weechatScripts/hotlist2extern.nix {};
        wttr = pkgs.callPackage packages/wttr.nix {};

        itl = pkgs.callPackage packages/itl.nix {};
        itools = pkgs.callPackage packages/itools.nix {itl = itl;};

        booksplit = wrapScript {
          script = inputs.voidrice.outPath + "/.local/bin/booksplit";
          name = "booksplit";
          packages = [pkgs.ffmpeg pkgs.glibc.bin];
        };
        dmenu-randr = pkgs.callPackage packages/dmenu-randr.nix {};
        tag = wrapScript {
          script = inputs.voidrice.outPath + "/.local/bin/tag";
          name = "tag";
          packages = [pkgs.ffmpeg];
        };
      };
    });
}
