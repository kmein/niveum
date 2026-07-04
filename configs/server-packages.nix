{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.vim
    pkgs.git
    pkgs.tmux
    pkgs.python3
  ];
}
