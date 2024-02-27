{ config, pkgs, inputs, niveumPackages, ... }:
let
  nextcloud = "${config.home.homeDirectory}/Nextcloud/ZODIAC";
  timeLedger = "${nextcloud}/hora.timeclock";
in
{
  home.packages = [
    pkgs.git
    (pkgs.writers.writeDashBin "hora" ''
      ${pkgs.hledger}/bin/hledger -f "${timeLedger}" "$@"
    '')
    (pkgs.writers.writeDashBin "hora-edit" ''
      nvim + "${timeLedger}"
    '')
    niveumPackages.vim
  ];

  home.sessionVariables.EDITOR = "${niveumPackages.vim}/bin/nvim";
  home.file."Local Applications".source = pkgs.symlinkJoin {
    name = "local-applications";
    paths = [ pkgs.anki-bin pkgs.dbeaver pkgs.vscode pkgs.mpv ];
  };
  home.stateVersion = "23.11";
  home.username = "xm7234fu";
  home.homeDirectory = "/Users/${config.home.username}";
  nixpkgs.config.allowUnfree = true;
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = "experimental-features = nix-command flakes";
}