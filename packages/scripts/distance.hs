{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Data.Char (toLower)
import Data.List (sortOn)
import Options.Applicative
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

data Options = Options
  { limit :: Int
  , word :: String
  , dictionary :: FilePath
  }

optionsParser :: Parser Options
optionsParser = do
  limit <- option auto (long "limit" <> short 'l' <> help "maximum edit distance to list" <> value 3 <> metavar "N")
  word <- strArgument (help "the word to match" <> metavar "WORD")
  dictionary <- strOption (long "dictionary" <> short 'd' <> help "the dictionary to search")
  pure Options {..}

readDictionary :: FilePath -> IO [String]
readDictionary path = lines . map toLower <$> readFile path

main :: IO ()
main = do
  let options = info (optionsParser <**> helper) (fullDesc <> progDesc "Find close words")
  Options {..} <- execParser options
  let word' = map toLower word
  allWords <- readDictionary dictionary
  let distances = map (levenshteinDistance defaultEditCosts word' &&& id) allWords
      distances' = distances `using` parList rdeepseq
      ranking = takeWhile ((<= limit) . fst) $ sortOn fst distances'
  forM_ ranking $ \(theDistance, theWord) -> putStrLn (show theDistance ++ " " ++ theWord)
