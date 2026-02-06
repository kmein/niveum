{
  pkgs,
  lib,
  config,
  ...
}:
{
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

  nixpkgs.overlays = [
    (final: prev: {
      niphas-editor = prev.niphas-editor.override {
        withCopilot = true;
        colorscheme = "gruvbox-dark-medium";
      };
    })
  ];

  environment.systemPackages = [
    pkgs.vim-typewriter
    pkgs.dawn-editor

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
