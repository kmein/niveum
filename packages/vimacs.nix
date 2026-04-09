{
  neovim,
  vimPlugins,
  tree-sitter,
  nodejs,
  chez,
  writers,
  lib,
  ...
}:
let
  vimacs = neovim.override {
    extraName = "-vimacs";
    configure = {
      customRC = ''
        source ${./vim-kmein/shared.vim}
        let g:conjure#client#scheme#stdio#command = "${lib.getExe' chez "petite"}"
        let g:conjure#client#scheme#stdio#prompt_pattern = "> $?"
      '';
      packages.nvim.start = [
        vimPlugins.parinfer-rust
        vimPlugins.conjure
        vimPlugins.orgmode
        vimPlugins.nvim-treesitter
        vimPlugins.nvim-treesitter-parsers.scheme
      ];
    };
  };
in
writers.writeDashBin "vimacs" ''
  export PATH="${lib.makeBinPath [ tree-sitter nodejs chez ]}:$PATH"
  ${vimacs}/bin/nvim "$@"
''
