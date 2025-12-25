{
  description = "niveum: packages, modules, systems";

  inputs = {
    self.submodules = true;

    agenix.url = "github:ryantm/agenix";
    autorenkalender.url = "github:kmein/autorenkalender";
    coptic-dictionary.url = "github:kmein/coptic-dictionary";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nixinate.url = "github:matthewcroughan/nixinate";
    nixpkgs-old.url = "github:NixOS/nixpkgs/50fc86b75d2744e1ab3837ef74b53f103a9b55a0";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nur.url = "github:nix-community/NUR";
    recht.url = "github:kmein/recht";
    retiolum.url = "github:krebs/retiolum";
    scripts.url = "github:kmein/scripts";
    stockholm.url = "github:krebs/stockholm";
    stylix.url = "github:danth/stylix/release-25.11";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";
    voidrice.url = "github:Lukesmithxyz/voidrice";
    wallpaper-generator.url = "github:pinpox/wallpaper-generator/v1.1";
    wallpapers.url = "github:kmein/wallpapers";

    agenix.inputs.home-manager.follows = "home-manager";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    autorenkalender.inputs.nixpkgs.follows = "nixpkgs";
    coptic-dictionary.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    menstruation-telegram.inputs.menstruation-backend.follows = "menstruation-backend";
    menstruation-telegram.inputs.nixpkgs.follows = "nixpkgs-old";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    recht.inputs.nixpkgs.follows = "nixpkgs";
    scripts.inputs.nixpkgs.follows = "nixpkgs";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";
    voidrice.flake = false;
    wallpapers.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      nur,
      home-manager,
      agenix,
      retiolum,
      nixinate,
      menstruation-backend,
      menstruation-telegram,
      scripts,
      tinc-graph,
      recht,
      autorenkalender,
      wallpaper-generator,
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
    in
    {
      apps = {
        x86_64-linux =
          let
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
            lib = nixpkgs.lib;
          in
          nixinate.nixinate.x86_64-linux self
          // {
            mock-secrets = {
              type = "app";
              program = toString (
                pkgs.writers.writeDash "mock-secrets" ''
                  ${pkgs.findutils}/bin/find secrets -not -path '*/.*' -type f  | ${pkgs.coreutils}/bin/sort > secrets.txt
                ''
              );
            };
          }
          # the following error prevents remote building of ful: https://github.com/NixOS/nixpkgs/issues/177873
          // builtins.listToAttrs (
            map (
              hostname:
              let
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
                program = toString (
                  pkgs.writers.writeDash "deploy-${hostname}" ''
                    exec ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch \
                      --max-jobs 2 \
                      --log-format internal-json \
                      --flake .#${hostname} \
                      --target-host ${targets.${hostname}} 2>&1 \
                      | ${pkgs.nix-output-monitor}/bin/nom --json
                  ''
                );
              }
            ) (builtins.attrNames self.nixosConfigurations)
          )
          // {
            deploy-ful = {
              type = "app";
              program = toString (
                pkgs.writers.writeDash "deploy-ful" ''
                  exec ${pkgs.nix}/bin/nix run .#nixinate.ful \
                    --log-format internal-json 2>&1 \
                    | ${pkgs.nix-output-monitor}/bin/nom --json
                ''
              );
            };
          };
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

      lib = {
        panoptikon = import lib/panoptikon.nix;
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

          niveumPackages = self.packages.${system};
          niveumLib = self.lib;
          inputs = {
            inherit tinc-graph self telebots menstruation-telegram menstruation-backend scripts agenix recht autorenkalender nixpkgs wallpaper-generator;
          };
        };
      in {
        ful = nixpkgs.lib.nixosSystem rec {
          system = "aarch64-linux";
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/ful/configuration.nix
            agenix.nixosModules.default
            self.nixosModules.passport
            self.nixosModules.panoptikon
            self.nixosModules.go-webring
            stockholm.nixosModules.reaktor2
            retiolum.nixosModules.retiolum
            nur.modules.nixos.default
            { nixpkgs.overlays = [ stockholm.overlays.default ]; }
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
            home-manager.nixosModules.home-manager
          ];
        };
        makanek = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          # for using inputs in other config files
          specialArgs = niveumSpecialArgs system;
          modules = [
            systems/makanek/configuration.nix
            self.nixosModules.telegram-bot
            self.nixosModules.passport
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
            nix-index-database.nixosModules.default
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
            nix-index-database.nixosModules.default
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
            nix-index-database.nixosModules.default
            stylix.nixosModules.stylix
          ];
        };
      };

      packages = eachSupportedSystem (system: let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            nur.overlays.default
            (self: super: {
              mpv = super.mpv.override {scripts = [super.mpvScripts.visualizer super.mpvScripts.mpris];};
              dmenu = super.writers.writeDashBin "dmenu" ''exec ${pkgs.rofi}/bin/rofi -dmenu "$@"'';
            })
          ];
        };
        wrapScript = {
          packages ? [],
          name,
          script,
        }:
          pkgs.writers.writeDashBin name ''PATH=$PATH:${nixpkgs.lib.makeBinPath (packages ++ [pkgs.findutils pkgs.coreutils pkgs.gnused pkgs.gnugrep])} ${script} "$@"'';
      in {
        # linguistics and ancient world
        auc = pkgs.callPackage packages/auc.nix {};
        betacode = pkgs.callPackage packages/betacode.nix {};
        brassica = pkgs.callPackage packages/brassica.nix {}; # TODO upstream
        devanagari = pkgs.callPackage packages/devanagari {};
        stardict-tools = pkgs.callPackage packages/stardict-tools.nix {};
        heuretes = pkgs.callPackage packages/heuretes.nix {};
        ipa = pkgs.writers.writePython3Bin "ipa" {flakeIgnore = ["E501"];} (builtins.readFile packages/ipa.py);
        jsesh = pkgs.callPackage packages/jsesh.nix {}; # TODO upstream
        kirciuoklis = pkgs.callPackage packages/kirciuoklis.nix {};
        polyglot = pkgs.callPackage packages/polyglot.nix {};
        tocharian-font = pkgs.callPackage packages/tocharian-font.nix {};
        gfs-fonts = pkgs.callPackage packages/gfs-fonts.nix {};
        closest = pkgs.callPackage packages/closest {};

        # lit
        random-zeno = pkgs.callPackage packages/random-zeno.nix {};
        literature-quote = pkgs.callPackage packages/literature-quote.nix {};

        # krebs
        brainmelter = pkgs.callPackage packages/brainmelter.nix {};
        cyberlocker-tools = pkgs.callPackage packages/cyberlocker-tools.nix {};
        hc = pkgs.callPackage packages/hc.nix {};
        kpaste = pkgs.callPackage packages/kpaste.nix {};
        pls = pkgs.callPackage packages/pls.nix {};
        untilport = pkgs.callPackage packages/untilport.nix {};
        radio-news = pkgs.callPackage packages/radio-news.nix {};

        # window manager
        swallow = pkgs.callPackage packages/swallow.nix {};
        devour = pkgs.callPackage packages/devour.nix {};

        cheat-sh = pkgs.callPackage packages/cheat-sh.nix {};
        vimPlugins-cheat-sh-vim = pkgs.callPackage packages/vimPlugins/cheat-sh.nix {}; # TODO upstream
        cro = pkgs.callPackage packages/cro.nix {};
        default-gateway = pkgs.callPackage packages/default-gateway.nix {};
        depp = pkgs.callPackage packages/depp.nix {};
        fkill = pkgs.callPackage packages/fkill.nix {};
        fzfmenu = pkgs.callPackage packages/fzfmenu.nix {};
        gpt35 = pkgs.callPackage packages/gpt.nix {model = "gpt-3.5-turbo";};
        gpt4 = pkgs.callPackage packages/gpt.nix {model = "gpt-4";};
        image-convert-favicon = pkgs.callPackage packages/image-convert-favicon.nix {};
        image-convert-tolino = pkgs.callPackage packages/image-convert-tolino.nix {};
        k-lock = pkgs.callPackage packages/k-lock.nix {};
        klem = pkgs.callPackage packages/klem.nix {};
        man-pandoc = pkgs.callPackage packages/man/pandoc.nix {}; # TODO upstream
        man-pdf = pkgs.callPackage packages/man-pdf.nix {};
        mansplain = pkgs.callPackage packages/mansplain.nix {};
        manual-sort = pkgs.callPackage packages/manual-sort.nix {};
        menu-calc = pkgs.callPackage packages/menu-calc.nix {};
        noise-waves = pkgs.callPackage packages/noise-waves.nix {};
        mpv-radio = pkgs.callPackage packages/mpv-radio.nix {di-fm-key-file = "/dev/null";};
        mpv-tuner = pkgs.callPackage packages/mpv-tuner.nix {di-fm-key-file = "/dev/null";};
        mpv-tv = pkgs.callPackage packages/mpv-tv.nix {};
        mpv-iptv = pkgs.callPackage packages/mpv-iptv.nix {};
        new-mac = pkgs.callPackage packages/new-mac.nix {};
        nix-git = pkgs.callPackage packages/nix-git.nix {};
        notemenu = pkgs.callPackage packages/notemenu.nix {niveumPackages = self.packages.${system};};
        opustags = pkgs.callPackage packages/opustags.nix {}; # TODO upstream
        q = pkgs.callPackage packages/q.nix {};
        qrpaste = pkgs.callPackage packages/qrpaste.nix {};
        go-webring = pkgs.callPackage packages/go-webring.nix {}; # TODO upstream
        rfc = pkgs.callPackage packages/rfc.nix {};
        gimp = pkgs.callPackage packages/gimp.nix {};
        scanned = pkgs.callPackage packages/scanned.nix {};
        text2pdf = pkgs.callPackage packages/text2pdf.nix {}; # TODO upstream
        timer = pkgs.callPackage packages/timer.nix {};
        trans = pkgs.callPackage packages/trans.nix {}; # TODO upstream
        ttspaste = pkgs.callPackage packages/ttspaste.nix {};
        unicodmenu = pkgs.callPackage packages/unicodmenu.nix {};
        emailmenu = pkgs.callPackage packages/emailmenu.nix {};
        stag = pkgs.callPackage packages/stag.nix {}; # TODO upstream
        vg = pkgs.callPackage packages/vg.nix {};
        vim = pkgs.callPackage packages/vim.nix {niveumPackages = self.packages.${system};};
        obsidian-vim = pkgs.callPackage packages/obsidian-vim.nix {};
        vimPlugins-icalendar-vim = pkgs.callPackage packages/vimPlugins/icalendar-vim.nix {}; # TODO upstream
        vimPlugins-jq-vim = pkgs.callPackage packages/vimPlugins/jq-vim.nix {}; # TODO upstream
        vimPlugins-typst-vim = pkgs.callPackage packages/vimPlugins/typst-vim.nix {}; # TODO upstream
        vimPlugins-mdwa-nvim = pkgs.callPackage packages/vimPlugins/mdwa-nvim.nix {}; # TODO upstream
        vimPlugins-vim-ernest = pkgs.callPackage packages/vimPlugins/vim-ernest.nix {}; # TODO upstream
        vimPlugins-vim-256noir = pkgs.callPackage packages/vimPlugins/vim-256noir.nix {}; # TODO upstream
        vimPlugins-vim-colors-paramount = pkgs.callPackage packages/vimPlugins/vim-colors-paramount.nix {}; # TODO upstream
        vimPlugins-vim-fetch = pkgs.callPackage packages/vimPlugins/vim-fetch.nix {}; # TODO upstream
        vimPlugins-vim-fsharp = pkgs.callPackage packages/vimPlugins/vim-fsharp.nix {}; # TODO upstream
        vimPlugins-vim-mail = pkgs.callPackage packages/vimPlugins/vim-mail.nix {}; # TODO upstream
        vimPlugins-vim-reason-plus = pkgs.callPackage packages/vimPlugins/vim-reason-plus.nix {}; # TODO upstream
        vimv = pkgs.callPackage packages/vimv.nix {};
        weechat-declarative = pkgs.callPackage packages/weechat-declarative.nix {}; # TODO upstream
        weechatScripts-hotlist2extern = pkgs.callPackage packages/weechatScripts/hotlist2extern.nix {}; # TODO upstream
        dmenu-randr = pkgs.callPackage packages/dmenu-randr.nix {};
        wttr = pkgs.callPackage packages/wttr.nix {}; # TODO upstream

        booksplit = wrapScript {
          script = voidrice.outPath + "/.local/bin/booksplit";
          name = "booksplit";
          packages = [pkgs.ffmpeg pkgs.glibc.bin];
        };
        tag = wrapScript {
          script = voidrice.outPath + "/.local/bin/tag";
          name = "tag";
          packages = [pkgs.ffmpeg];
        };
      });
    };
}
