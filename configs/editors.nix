{ pkgs, config, ... }:
let vimConfig = import ../dot/vim.nix { inherit config pkgs; };
in {
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.view = "nvim -R";

  programs.nano.nanorc = ''
    set autoindent
    set boldtext
    set morespace
    set smarthome
    set tabsize 4
    set tabstospaces
  '';

  nixpkgs.config.packageOverrides = pkgs: {
    nvim = pkgs.neovim.override {
      configure = {
        customRC = vimConfig.vimrc;
        packages.nvim = with pkgs.vimPlugins; {
          start = vimConfig.startPackages;
          opt = [
            elm-vim
            deoplete-rust
            idris-vim
            vimtex
            rust-vim
            typescript-vim
            vim-javascript
            vim-nix
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "vim-ledger";
              src = pkgs.fetchFromGitHub {
                owner = "ledger";
                repo = "vim-ledger";
                rev = "6eb3bb21aa979cc295d0480b2179938c12b33d0d";
                sha256 = "0rbwyaanvl2bqk8xm4kq8fkv8y92lpf9xx5n8gw54iij7xxhnj01";
              };
            })
            (pkgs.vimUtils.buildVimPluginFrom2Nix {
              name = "vim-fsharp";
              src = pkgs.fetchFromGitHub {
                owner = "fsharp";
                repo = "vim-fsharp";
                rev = "627db7d701747e8fd7924b6505c61e16a369fb72";
                sha256 = "00hhgn2p54faysx1ddccyhl9jnvddgxsczhv0np3mgzza6ls4838";
              };
            })
          ];
        };
      };
    };
  };

  environment.systemPackages = [pkgs.nvim];
}
