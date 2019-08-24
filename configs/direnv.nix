{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.direnv ];

  home-manager.users.me.programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    stdlib = builtins.readFile ("${pkgs.fetchFromGitHub {
      owner = "Mic92";
      repo = "dotfiles";
      rev = "a0a9b7e358fa70a85cd468f8ca1fbb02ae0a91df";
      sha256 = "1y9h5s1lf59sczsm0ksq2x1yhl98ba9lwk5yil3q53rg7n4574pg";
    }}/home/.direnvrc");
  };

  programs.zsh.interactiveShellInit = ''
    eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
  '';
}
