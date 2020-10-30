{ config, pkgs, ... }: {
  home-manager.users.me.home.file = {
    ".ghc/ghci.conf".text = ''
      :set editor vim
      :def hoogle \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --count=15 \"" ++ s ++ "\""
      :def doc \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --info \"" ++ s ++ "\""
      :set prompt "\o033[1m%s\o033[1;34m λ\o033[0m "
      :set -Wall
      :set -XOverloadedStrings
    '';
    # :def unpl \x -> return $ ":!${pkgs.haskellPackages.pointful}/bin/pointful \"" ++ x ++ "\""
      # :def pl \x -> return $ ":!${pkgs.haskellPackages.pointfree}/bin/pointfree -v \"" ++ x ++ "\""
    ".stack/config.yaml".text = let inherit (import <niveum/lib>) kieran;
    in builtins.toJSON {
      templates.params = {
        author-name = kieran.name;
        author-email = kieran.email;
        copyright = "Copyright: (c) 2020 ${kieran.name}";
        github-username = kieran.github;
      };
    };
  };

  services.hoogle = {
    enable = false;
    packages = import ./packages.nix;
    port = 8091;
  };

  environment.systemPackages = with pkgs;
    [
      cabal2nix
      cabal-install
      hlint
      haskellPackages.brittany
      # haskellPackages.hfmt
      (haskellPackages.ghcWithHoogle (import ./packages.nix))
    ] ++ map haskell.lib.justStaticExecutables [
      haskellPackages.ghcid
      haskellPackages.hasktags
      # haskellPackages.hindent
      # haskellPackages.pointfree
      # haskellPackages.pointful
      haskellPackages.hpack
    ];
}
