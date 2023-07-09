{
  neovim,
  vimPlugins,
  vault ? "~/cloud/syncthing/obsidian/",
  ...
}:
neovim.override {
  configure = {
    customRC = ''
      let g:vimwiki_list = [{'path': '${vault}',
        \ 'syntax': 'markdown', 'ext': '.md'}]
    '';
    packages.nvim.start = [
      vimPlugins.vimwiki
    ];
  };
}
