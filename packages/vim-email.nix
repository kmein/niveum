{
  neovim,
  vimPlugins,
  writers,
  ...
}:
let
  vim-email = neovim.override {
    extraName = "-email";
    configure = {
      customRC = ''
        source ${./vim-kmein/shared.vim}
        autocmd VimEnter * Himalaya
      '';
      packages.nvim.start = [
        vimPlugins.telescope-nvim
        vimPlugins.himalaya-vim
      ];
    };
  };
in
  vim-email
