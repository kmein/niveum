{ config, pkgs, ... }:
{
  home-manager.users.me.home.file = {
    ".ghc/ghci.conf".text = ''
      :set editor vim
      :def hoogle \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --count=15 \"" ++ s ++ "\""
      :def doc \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --info \"" ++ s ++ "\""
      :def pl \x -> return $ ":!${pkgs.haskellPackages.pointfree}/bin/pointfree -v \"" ++ x ++ "\""
      :def unpl \x -> return $ ":!${pkgs.haskellPackages.pointful}/bin/pointful \"" ++ x ++ "\""
      :set prompt "\o033[1m%s\o033[1;34m λ\o033[0m "
      :set -Wall
    '';
    ".stack/config.yaml".text = let user = config.niveum.user; in ''
      templates:
        params:
          author-name: ${user.name}
          author-email: ${user.email}
          copyright: 'Copyright: (c) 2019 ${user.name}'
          github-username: ${user.github}
    '';
  };

  services.hoogle = {
    enable = true;
    packages = import ./packages.nix;
  };

  environment.systemPackages = with pkgs; [
    cabal2nix
    stack2nix
    cabal-install
    hlint
    haskellPackages.brittany
    (haskellPackages.ghcWithHoogle (import ./packages.nix))
  ] ++ map haskell.lib.justStaticExecutables [
    haskellPackages.ghcid
    haskellPackages.hakyll
    haskellPackages.hfmt
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.pointfree
    haskellPackages.pointful
    haskellPackages.hpack
  ];
}
