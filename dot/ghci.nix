{pkgs}:
''
:set editor vim
:def hoogle \x -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle --color \"" ++ x ++ "\" "
:def doc \x -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle --info --color \"" ++ x ++ "\""
:def search \x -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle --color \"" ++ x ++ "\" | head"
:def pl \x -> return $ ":!${pkgs.haskellPackages.pointfree}/bin/pointfree -v \"" ++ x ++ "\""
:def unpl \x -> return $ ":!${pkgs.haskellPackages.pointful}/bin/pointful \"" ++ x ++ "\""
:set prompt "\o033[1m%s\o033[1;34m λ\o033[0m "
''
# :def djinn \x -> return $ ":!echo \"" ++ x ++ "\" | ${pkgs.haskell.packages.ghc7102.djinn}/bin/djinn /dev/stdin"
