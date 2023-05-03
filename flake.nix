{
  description = "niveum: packages, modules, systems";

  inputs = {
    agenix.url = "github:ryantm/agenix";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    menstruation-backend.url = "github:kmein/menstruation.rs";
    menstruation-telegram.url = "github:kmein/menstruation-telegram";
    nixinate.url = "github:matthewcroughan/nixinate";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nur.url = "github:nix-community/NUR";
    recht.url = "github:kmein/recht";
    scripts.url = "github:kmein/scripts";
    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    telebots.url = "github:kmein/telebots";
    tinc-graph.url = "github:kmein/tinc-graph";
    traadfri.url = "github:kmein/traadfri";
    voidrice.url = "github:Lukesmithxyz/voidrice";

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    menstruation-backend.inputs.flake-utils.follows = "flake-utils";
    menstruation-backend.inputs.nixpkgs.follows = "nixpkgs";
    nixinate.inputs.nixpkgs.follows = "nixpkgs";
    tinc-graph.inputs.flake-utils.follows = "flake-utils";
    tinc-graph.inputs.nixpkgs.follows = "nixpkgs";
    voidrice.flake = false;
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nur,
    home-manager,
    nixinate,
    agenix,
    retiolum,
    flake-utils,
    ...
  }:
    {
      apps =
        nixinate.nixinate.x86_64-linux self
        // {
          x86_64-linux.deploy = let
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
          in {
            type = "app";
            program = toString (pkgs.writers.writeDash "deploy" ''
              if [ $# -eq 0 ]
              then
                systems='${toString (builtins.attrNames self.nixosConfigurations)}'
              else
                systems=$*
              fi
              ${pkgs.parallel}/bin/parallel --line-buffer --tagstring '{}' 'nix run .\?submodules=1\#apps.nixinate.{}' ::: $systems
            '');
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
        traadfri = import modules/traadfri.nix;
      };

      lib = {
        panoptikon = import lib/panoptikon.nix;
      };

      nixosConfigurations = {
        ful = nixpkgs.lib.nixosSystem rec {
          system = "aarch64-linux";
          specialArgs = {
            niveumPackages = inputs.self.packages.${system};
            niveumLib = inputs.self.lib;
            inherit inputs;
          };
          modules = [
            {
              _module.args.nixinate = {
                host = "ful";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
            systems/ful/configuration.nix
            agenix.nixosModules.default
            inputs.self.nixosModules.passport
            inputs.self.nixosModules.panoptikon
            retiolum.nixosModules.retiolum
            nur.nixosModules.nur
          ];
        };
        zaatar = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            niveumPackages = inputs.self.packages.${system};
            inherit inputs;
          };
          modules = [
            {
              _module.args.nixinate = {
                host = "zaatar";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
            systems/zaatar/configuration.nix
            inputs.self.nixosModules.moodle-dl
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        makanek = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          # for using inputs in other config files
          specialArgs = {
            niveumPackages = inputs.self.packages.${system};
            inherit inputs;
          };
          modules = [
            {
              _module.args.nixinate = {
                host = "makanek";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
            systems/makanek/configuration.nix
            inputs.self.nixosModules.telegram-bot
            inputs.self.nixosModules.htgen
            inputs.self.nixosModules.passport
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            nur.nixosModules.nur
          ];
        };
        tahina = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            systems/tahina/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        tabula = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            systems/tabula/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
          ];
        };
        manakish = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            niveumPackages = inputs.self.packages.${system};
            inherit inputs;
          };
          modules = [
            {
              _module.args.nixinate = {
                host = "manakish";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
            systems/manakish/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            home-manager.nixosModules.home-manager
            nur.nixosModules.nur
          ];
        };
        kabsa = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = {
            niveumPackages = inputs.self.packages.${system};
            inherit inputs;
          };
          modules = [
            {
              _module.args.nixinate = {
                host = "kabsa";
                sshUser = "root";
                buildOn = "remote";
                substituteOnTarget = true;
                hermetic = false;
              };
            }
            systems/kabsa/configuration.nix
            agenix.nixosModules.default
            retiolum.nixosModules.retiolum
            home-manager.nixosModules.home-manager
            nur.nixosModules.nur
          ];
        };
      };
    }
    // flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux flake-utils.lib.system.aarch64-linux] (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          nur.overlay
          (self: super: {
            mpv = super.mpv.override {scripts = [inputs.self.packages.${system}.mpv-visualizer];};
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
      packages = rec {
        auc = pkgs.callPackage packages/auc.nix {};
        betacode = pkgs.callPackage packages/betacode.nix {};
        cheat-sh = pkgs.callPackage packages/cheat-sh.nix {};
        closest = pkgs.callPackage packages/closest {};
        cyberlocker-tools = pkgs.callPackage packages/cyberlocker-tools.nix {};
        default-gateway = pkgs.callPackage packages/default-gateway.nix {};
        depp = pkgs.callPackage packages/depp.nix {};
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
        gpt = pkgs.callPackage packages/gpt.nix {};
        hc = pkgs.callPackage packages/hc.nix {};
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
        mpv-radio = pkgs.callPackage packages/mpv-radio.nix {di-fm-key-file = "/dev/null";};
        mpv-tv = pkgs.callPackage packages/mpv-tv.nix {};
        mpv-visualizer = pkgs.callPackage packages/mpv-visualizer.nix {};
        new-mac = pkgs.callPackage packages/new-mac.nix {};
        nix-git = pkgs.callPackage packages/nix-git.nix {};
        nix-index-update = pkgs.callPackage packages/nix-index-update.nix {inherit system;};
        opustags = pkgs.callPackage packages/opustags.nix {};
        pls = pkgs.callPackage packages/pls.nix {};
        qrpaste = pkgs.callPackage packages/qrpaste.nix {};
        rfc = pkgs.callPackage packages/rfc.nix {};
        scanned = pkgs.callPackage packages/scanned.nix {};
        swallow = pkgs.callPackage packages/swallow.nix {};
        text2pdf = pkgs.callPackage packages/text2pdf.nix {};
        timer = pkgs.callPackage packages/timer.nix {};
        tocharian-font = pkgs.callPackage packages/tocharian-font.nix {};
        trans = pkgs.callPackage packages/trans.nix {};
        ttspaste = pkgs.callPackage packages/ttspaste.nix {};
        unicodmenu = pkgs.callPackage packages/unicodmenu.nix {};
        untilport = pkgs.callPackage packages/untilport.nix {};
        vg = pkgs.callPackage packages/vg.nix {};
        vimPlugins-cheat-sh-vim = pkgs.callPackage packages/vimPlugins/cheat-sh.nix {};
        vimPlugins-icalendar-vim = pkgs.callPackage packages/vimPlugins/icalendar-vim.nix {};
        vimPlugins-jq-vim = pkgs.callPackage packages/vimPlugins/jq-vim.nix {};
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
        dmenu-randr = wrapScript {
          script = inputs.voidrice.outPath + "/.local/bin/displayselect";
          name = "dmenu-randr";
          packages = [pkgs.dmenu pkgs.bc pkgs.psmisc pkgs.util-linux pkgs.xorg.xrandr pkgs.gawk pkgs.libnotify pkgs.arandr (pkgs.writers.writeDashBin "setbg" "")];
        };
        tag = wrapScript {
          script = inputs.voidrice.outPath + "/.local/bin/tag";
          name = "tag";
          packages = [pkgs.ffmpeg];
        };
      };
    });
}
