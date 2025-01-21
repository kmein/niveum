{
  pkgs,
  niveumPackages,
  config,
  ...
}: {
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
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
    (pkgs.writers.writeDashBin "vim" ''neovim "$@"'')
    (niveumPackages.vim.override {
      stylixColors = config.lib.stylix.colors;
      colorscheme = "base16-gruvbox-dark-medium";
    })

    # language servers
    pkgs.pyright
    pkgs.haskellPackages.haskell-language-server
    pkgs.texlab
    pkgs.nil
    pkgs.rust-analyzer
    pkgs.nodePackages.typescript-language-server
    pkgs.lua-language-server
    pkgs.nodePackages.vscode-langservers-extracted
    pkgs.lemminx
    niveumPackages.jq-lsp
    pkgs.dhall-lsp-server
  ];
}
