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
          opt = vimConfig.optPackages;
        };
      };
    };
  };

  environment.systemPackages = [pkgs.nvim];
}
