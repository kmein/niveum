# local packages; the flat name -> callPackage map lives in ../packages/default.nix
final: prev:
import ../packages prev
// {
  weechatScripts = prev.weechatScripts // {
    hotlist2extern = prev.callPackage ../packages/weechatScripts/hotlist2extern.nix { }; # TODO upstream
  };
  vimPlugins = prev.vimPlugins // {
    cheat-sh = prev.callPackage ../packages/vimPlugins/cheat-sh.nix { };
    icalendar-vim = prev.callPackage ../packages/vimPlugins/icalendar-vim.nix { }; # TODO upstream
    jq-vim = prev.callPackage ../packages/vimPlugins/jq-vim.nix { }; # TODO upstream
    typst-vim = prev.callPackage ../packages/vimPlugins/typst-vim.nix { }; # TODO upstream
    mdwa-nvim = prev.callPackage ../packages/vimPlugins/mdwa-nvim.nix { }; # TODO upstream
    vim-ernest = prev.callPackage ../packages/vimPlugins/vim-ernest.nix { }; # TODO upstream
    vim-256noir = prev.callPackage ../packages/vimPlugins/vim-256noir.nix { }; # TODO upstream
    vim-colors-paramount = prev.callPackage ../packages/vimPlugins/vim-colors-paramount.nix { }; # TODO upstream
    vim-fetch = prev.callPackage ../packages/vimPlugins/vim-fetch.nix { }; # TODO upstream
    vim-fsharp = prev.callPackage ../packages/vimPlugins/vim-fsharp.nix { }; # TODO upstream
    vim-mail = prev.callPackage ../packages/vimPlugins/vim-mail.nix { }; # TODO upstream
    vim-reason-plus = prev.callPackage ../packages/vimPlugins/vim-reason-plus.nix { }; # TODO upstream
  };
}
