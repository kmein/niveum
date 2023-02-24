{writers}:
writers.writeHaskellBin "manual-sort" {} ''
  {-# LANGUAGE LambdaCase #-}
  import Data.Char (toLower)
  import System.Environment (getArgs)
  import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

  insertionSortM :: Monad f => (a -> a -> f Ordering) -> [a] -> f [a]
  insertionSortM cmp = foldr ((=<<) . insertByM cmp) (pure [])
   where
    insertByM cmp x = \case
      [] -> pure [x]
      yys@(y : ys) -> cmp x y >>= \case
        GT -> (y :) <$> insertByM cmp x ys
        _ -> pure (x : yys)

  ask :: Show a => a -> a -> IO Ordering
  ask a b = do
    putStr (show a ++ " > " ++ show b ++ "? (y/n) ")
    map toLower <$> getLine >>= \case
      'y' : _ -> return GT
      _ -> return LT

  main :: IO ()
  main = do
    hSetBuffering stdout NoBuffering
    argv <- getArgs
    sorted <- insertionSortM ask argv
    mapM_ (\(place, thing) -> putStrLn (show place ++ ". " ++ show thing)) $ zip [1 ..] (reverse sorted)
''
