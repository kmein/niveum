{ config, pkgs, ... }:
let
  executables = pkgs.haskell.lib.justStaticExecutables;
  haskells = import ../dot/haskells.nix;
in {
  users.users.kfm.packages = with pkgs; [
    (haskellPackages.ghcWithHoogle haskells)
    (executables haskellPackages.cabal-install)
    (executables haskellPackages.ghcid)
    (executables haskellPackages.hasktags)
    (executables haskellPackages.hindent)
    (executables haskellPackages.pointfree)
    (executables haskellPackages.pointful)
    (executables haskellPackages.hlint)
    (executables haskellPackages.hpack)
  ];

  home-manager.users.kfm.home.file = {
    ".ghc/ghci.conf".text = ''
      :set editor vim
      :def hoogle \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --count=15 \"" ++ s ++ "\""
      :def doc \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --info \"" ++ s ++ "\""
      :def pl \x -> return $ ":!${pkgs.haskellPackages.pointfree}/bin/pointfree -v \"" ++ x ++ "\""
      :def unpl \x -> return $ ":!${pkgs.haskellPackages.pointful}/bin/pointful \"" ++ x ++ "\""
      :set prompt "\o033[1m%s\o033[1;34m λ\o033[0m "
    '';
    ".stack/config.yaml".text = let user = config.constants.user; in ''
      templates:
        params:
          author-name: ${user.name}
          author-email: ${user.email}
          copyright: 'Copyright: (c) 2018 ${user.name}'
          github-username: ${user.github}
    '';
  };
}
