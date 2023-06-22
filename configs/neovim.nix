{
  pkgs,
  niveumPackages,
  ...
}: {
  environment.variables.EDITOR = pkgs.lib.mkForce "nvim";
  environment.shellAliases.vi = "nvim";
  environment.shellAliases.vim = "nvim";
  environment.shellAliases.view = "nvim -R";

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "vim" ''neovim "$@"'')
    niveumPackages.vim
  ];
}
