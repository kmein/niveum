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
      coptic-dictionary,
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
          lib.mergeAttrsList [
            (nixinate.nixinate.x86_64-linux self)
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
            # the following error prevents remote building of ful: https://github.com/NixOS/nixpkgs/issues/177873
            (builtins.listToAttrs (
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
            ))
            {
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
            }
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

      lib = {
      };

      overlays.default = final: prev: {
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
          vim-colors-paramount =
            prev.callPackage packages/vimPlugins/vim-colors-paramount.nix { }; # TODO upstream
          vim-fetch = prev.callPackage packages/vimPlugins/vim-fetch.nix { }; # TODO upstream
          vim-fsharp = prev.callPackage packages/vimPlugins/vim-fsharp.nix { }; # TODO upstream
          vim-mail = prev.callPackage packages/vimPlugins/vim-mail.nix { }; # TODO upstream
          vim-reason-plus = prev.callPackage packages/vimPlugins/vim-reason-plus.nix { }; # TODO upstream
        };

        # krebs
        brainmelter = prev.callPackage packages/brainmelter.nix { };
        cyberlocker-tools = prev.callPackage packages/cyberlocker-tools.nix { };
        hc = prev.callPackage packages/hc.nix { };
        pls = prev.callPackage packages/pls.nix { };
        radio-news = prev.callPackage packages/radio-news.nix { };
        untilport = prev.callPackage packages/untilport.nix { };
        weechat-declarative = prev.callPackage packages/weechat-declarative {};

        # my packages
        betacode = prev.callPackage packages/betacode.nix { };
        closest = prev.callPackage packages/closest { };
        default-gateway = prev.callPackage packages/default-gateway.nix { };
        depp = prev.callPackage packages/depp.nix { };
        devanagari = prev.callPackage packages/devanagari { };
        radioStreams = prev.callPackage packages/streams {};
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
        polyglot = prev.callPackage packages/polyglot.nix { };
        q = prev.callPackage packages/q.nix { };
        qrpaste = prev.callPackage packages/qrpaste.nix { };
        random-zeno = prev.callPackage packages/random-zeno.nix { };
        scanned = prev.callPackage packages/scanned.nix { };
        stardict-tools = prev.callPackage packages/stardict-tools.nix { };
        swallow = prev.callPackage packages/swallow.nix { };
        tocharian-font = prev.callPackage packages/tocharian-font.nix { };
        ttspaste = prev.callPackage packages/ttspaste.nix { };
        unicodmenu = prev.callPackage packages/unicodmenu.nix { };
        vg = prev.callPackage packages/vg.nix { };
        vim-kmein = prev.callPackage packages/vim-kmein {};
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
          niveumSpecialArgs = system: {
            unstablePackages = import nixpkgs-unstable {
              inherit system;
              overlays = [];
              config.allowUnfreePredicate =
                pkg:
                builtins.elem (nixpkgs-unstable.lib.getName pkg) [
                  "obsidian"
                  "zoom"
                ];
            };
            inputs = {
              inherit
                tinc-graph
                self
                telebots
                menstruation-telegram
                menstruation-backend
                scripts
                coptic-dictionary
                agenix
                recht
                autorenkalender
                nixpkgs
                wallpaper-generator
                ;
            };
          };
        in
        {
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
              { nixpkgs.overlays = [ self.overlays.default ]; }
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
        in
        {
          inherit (pkgs) auc swallow cheat-sh hc kpaste noise-waves trans stag qrpaste new-mac scanned default-gateway kirciuoklis tocharian-font image-convert-favicon image-convert-tolino heuretes mpv-tv mpv-iptv devanagari literature-quote booksplit manual-sort wttr emailmenu closest mpv-radio mpv-tuner cro nix-git text2pdf betacode brassica ipa polyglot jsesh gfs-fonts vim-kmein vimv brainmelter cyberlocker-tools pls untilport radio-news vg ttspaste depp fkill fzfmenu unicodmenu dmenu-randr notemenu man-pdf mansplain opustags q timer rfc gimp obsidian-vim devour go-webring random-zeno stardict-tools weechat-declarative klem radioStreams;
        }
      );
    };
}
