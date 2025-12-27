{
  writers,
  haskell,
  haskellPackages,
}:
writers.writeHaskellBin "betacode"
  {
    libraries = [
      (haskell.lib.unmarkBroken (haskell.lib.doJailbreak haskellPackages.betacode))
      haskellPackages.text
    ];
  }
  ''
    import qualified Data.Text.IO as T
    import qualified Data.Text as T
    import Text.BetaCode
    main = T.interact (either (error . T.unpack) id . fromBeta)
  ''
