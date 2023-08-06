{
  pkgs,
  niveumPackages,
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
    niveumPackages.vim
  ];
}
