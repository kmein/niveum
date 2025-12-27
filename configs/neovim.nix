{
  pkgs,
  lib,
  config,
  ...
}: let
  vim-kmein = (pkgs.vim-kmein.override {
    # stylixColors = config.lib.stylix.colors;
    colorscheme = "base16-gruvbox-dark-medium";
  });
in {
  environment.variables.EDITOR = lib.getExe vim-kmein;
  environment.shellAliases.view = "nvim -R";

  home-manager.users.me = {
    editorconfig = {
      enable = true;
      settings = {
        "*" = {
          charset = "utf-8";
          end_of_line = "lf";
          trim_trailing_whitespace = true;
          insert_final_newline = true;
          indent_style = "space";
          indent_size = 2;
        };
        "*.py" = {
          indent_size = 4;
        };
        Makefile = {
          indent_style = "tab";
        };
        "*.md" = {
          trim_trailing_whitespace = false;
        };
      };
    };
  };

  environment.systemPackages = [
    pkgs.vim-typewriter
    vim-kmein

    # language servers
    pkgs.pyright
    pkgs.haskellPackages.haskell-language-server
    pkgs.texlab
    pkgs.nil
    pkgs.gopls
    pkgs.nixfmt-rfc-style
    pkgs.rust-analyzer
    pkgs.nodePackages.typescript-language-server
    pkgs.lua-language-server
    pkgs.nodePackages.vscode-langservers-extracted
    pkgs.lemminx # XML LSP
    pkgs.jq-lsp
    pkgs.dhall-lsp-server
  ];
}
