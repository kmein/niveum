{
  haskell,
  haskellPackages,
}:
haskell.lib.dontCheck (haskell.lib.unmarkBroken (haskell.lib.doJailbreak haskellPackages.brassica))
