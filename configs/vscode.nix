{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.vscode-with-extensions.override {
      vscodeExtensions = [
        pkgs.vscode-extensions.bbenoist.Nix
      ];
    })
  ];
}
