{ pkgs }:
''
:set editor vim
:def hoogle \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --count=15 \"" ++ s ++ "\""
:def doc \s -> return $ ":!${pkgs.haskellPackages.hoogle}/bin/hoogle search --color -l --info \"" ++ s ++ "\""
:def pl \x -> return $ ":!${pkgs.haskellPackages.pointfree}/bin/pointfree -v \"" ++ x ++ "\""
:def unpl \x -> return $ ":!${pkgs.haskellPackages.pointful}/bin/pointful \"" ++ x ++ "\""
:set prompt "\o033[1m%s\o033[1;34m λ\o033[0m "
''
# :def djinn \x -> return $ ":!echo \"" ++ x ++ "\" | ${pkgs.haskell.packages.ghc7102.djinn}/bin/djinn /dev/stdin"
