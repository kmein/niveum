{-# LANGUAGE OverloadedStrings #-}

-- downloads the audio of every starred miniflux entry into gpodder-style
-- folders in the current directory. the api is only ever read.

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight)
import Data.Char (isSpace, toLower)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import Data.IORef
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, statusCode)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath (takeDirectory, (</>))
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

-- config

parseEnv :: Text -> M.Map Text Text
parseEnv = M.fromList . mapMaybe line . T.lines
  where
    line l
      | T.null s || "#" `T.isPrefixOf` s || T.null v = Nothing
      | otherwise = Just (T.strip k, T.strip (T.drop 1 v))
      where
        s = T.strip l
        (k, v) = T.breakOn "=" s

loadEnv :: IO (Text, Text)
loadEnv = do
  has <- doesFileExist ".env"
  file <- if has then parseEnv <$> TIO.readFile ".env" else pure M.empty
  let get k = do
        e <- lookupEnv k
        pure $ mfilter (not . T.null) (T.pack <$> e <|> M.lookup (T.pack k) file)
  url <- get "MINIFLUX_URL"
  token <- get "MINIFLUX_TOKEN"
  case (url, token) of
    (Just u, Just t) -> pure (fromMaybe u (T.stripSuffix "/" u), t)
    _ -> failWith "missing MINIFLUX_URL / MINIFLUX_TOKEN (environment or ./.env)"

failWith :: Text -> IO a
failWith msg = do
  tty <- hIsTerminalDevice stderr
  TIO.hPutStrLn stderr (colour tty "31" msg)
  exitWith (ExitFailure 2)

-- api

data Enclosure = Enclosure {encUrl :: Text, encMime :: Text}

data Entry = Entry {eId :: Int, eTitle :: Text, eFeed :: Text, eEnclosures :: [Enclosure]}

data Page = Page {pTotal :: Int, pEntries :: [Entry]}

instance FromJSON Enclosure where
  parseJSON = withObject "enclosure" $ \o ->
    Enclosure <$> o .: "url" <*> o .:? "mime_type" .!= ""

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o ->
    Entry
      <$> o .: "id"
      <*> o .: "title"
      <*> (o .: "feed" >>= withObject "feed" (.: "title"))
      -- null on most entries
      <*> o .:? "enclosures" .!= []

instance FromJSON Page where
  parseJSON = withObject "page" $ \o -> Page <$> o .: "total" <*> o .: "entries"

starred :: Manager -> Text -> Text -> IO [Entry]
starred mgr base token = go []
  where
    go acc = do
      let off = length acc
      req <- parseRequest . T.unpack $
        base <> "/entries?starred=true&order=published_at&direction=asc&limit=500&offset=" <> T.pack (show off)
      res <- httpLbs req {requestHeaders = [("X-Auth-Token", TE.encodeUtf8 token), ("Accept", "application/json")]} mgr
      let code = statusCode (responseStatus res)
      when (code /= 200) . failWith $
        "miniflux: " <> T.pack (show code) <> " " <> TE.decodeUtf8With TE.lenientDecode (BL.toStrict (BL.take 200 (responseBody res)))
      page <- either (failWith . ("miniflux: " <>) . T.pack) pure (eitherDecode (responseBody res))
      let acc' = acc ++ pEntries page
      if null (pEntries page) || length acc' >= pTotal page then pure acc' else go acc'

-- selecting and naming

audioExts :: [Text]
audioExts = ["mp3", "m4a", "m4b", "aac", "ogg", "oga", "opus", "flac", "wav"]

urlExt :: Text -> Maybe Text
urlExt url
  | T.null stem = Nothing
  | e `elem` audioExts = Just e
  | otherwise = Nothing
  where
    path = T.takeWhile (`notElem` ("?#" :: String)) url
    (stem, rawExt) = T.breakOnEnd "." (last (T.splitOn "/" path))
    e = T.toLower rawExt

mime :: Enclosure -> Text
mime = T.toLower . T.strip . T.takeWhile (/= ';') . encMime

isAudio :: Enclosure -> Bool
isAudio enc =
  "audio/" `T.isPrefixOf` m || (m == "application/octet-stream" && isJust (urlExt (encUrl enc)))
  where
    m = mime enc

ext :: Enclosure -> Text
ext enc = fromMaybe fromMime (urlExt (encUrl enc))
  where
    m = mime enc
    sub = T.drop 1 (snd (T.breakOn "/" m))
    fromMime = fromMaybe (fromMaybe sub (T.stripPrefix "x-" sub)) (lookup m mimeExts)

mimeExts :: [(Text, Text)]
mimeExts =
  [ ("audio/mpeg", "mp3"), ("audio/mp3", "mp3"),
    ("audio/mp4", "m4a"), ("audio/x-m4a", "m4a"), ("audio/m4a", "m4a"), ("audio/aac", "m4a"),
    ("audio/ogg", "ogg"), ("audio/opus", "opus"),
    ("audio/wav", "wav"), ("audio/x-wav", "wav"), ("audio/wave", "wav"),
    ("audio/flac", "flac"), ("audio/x-flac", "flac")
  ]

-- same rules as gpodder's sanitize_filename, so files land in its folders
sanitize :: Int -> Text -> Text
sanitize n =
  T.dropAround (\c -> c == '.' || isSpace c)
    . T.map (\c -> if c `elem` ("\"*/:<>?\\|" :: String) then '_' else c)
    . T.take n
    . T.map (\c -> if c < ' ' then ' ' else c)

orElse :: Text -> Text -> Text
orElse fallback t = if T.null t then fallback else t

-- relative paths for an entry's audio, given the paths taken so far this run
entryPaths :: S.Set FilePath -> Entry -> ([(Enclosure, FilePath)], S.Set FilePath)
entryPaths taken0 e = foldl step ([], taken0) (zip [1 :: Int ..] audio)
  where
    audio = filter isAudio (eEnclosures e)
    folder = orElse "miniflux" (sanitize 60 (eFeed e))
    stem = orElse ("entry-" <> T.pack (show (eId e))) (sanitize 120 (eTitle e))
    path s enc = T.unpack folder </> T.unpack (s <> "." <> ext enc)
    step (acc, taken) (i, enc) =
      let s = if length audio > 1 then stem <> " (" <> T.pack (show i) <> ")" else stem
          p0 = path s enc
          p = if p0 `S.member` taken then path (s <> " [" <> T.pack (show (eId e)) <> "]") enc else p0
       in (acc ++ [(enc, p)], S.insert p taken)

-- ledger: entry ids, never downloaded again even once deleted.
-- enclosure ids would not do: miniflux recreates them when episode urls change.

ledgerFile :: FilePath
ledgerFile = ".miniflux-downloaded"

readLedger :: IO (S.Set Int)
readLedger = do
  has <- doesFileExist ledgerFile
  if has
    then S.fromList . mapMaybe (readMaybe . T.unpack . T.strip) . T.lines <$> TIO.readFile ledgerFile
    else pure S.empty

record :: Int -> IO ()
record i = appendFile ledgerFile (show i ++ "\n")

-- downloading

data Job = Job {jEntry :: Int, jUrl :: Text, jPath :: FilePath}

-- the byte count, or why the file failed
download :: Manager -> IORef Int -> Job -> IO (Either Text Int)
download mgr bytes job = do
  r <- try fetch
  case r of
    Right (Right n) -> pure (Right n)
    Right (Left msg) -> cleanup >> pure (Left msg)
    Left e
      | isJust (fromException e :: Maybe SomeAsyncException) -> cleanup >> throwIO e
      | otherwise -> cleanup >> pure (Left (T.takeWhile (/= '\n') (T.pack (displayException e))))
  where
    part = jPath job ++ ".part"
    cleanup = void (try (removeFile part) :: IO (Either SomeException ()))
    fetch = do
      req <- parseRequest (T.unpack (jUrl job))
      withResponse req {requestHeaders = ("User-Agent", "fetch-podcasts") : requestHeaders req} mgr $ \res -> do
        let code = statusCode (responseStatus res)
            ctype = maybe "" (BC.map toLower) (lookup hContentType (responseHeaders res))
        if code < 200 || code >= 300
          then pure (Left ("HTTP " <> T.pack (show code)))
          -- some feeds link the episode's web page instead of its audio
          else if "text/html" `B.isPrefixOf` ctype
          then pure (Left "not audio: text/html")
          else do
            createDirectoryIfMissing True (takeDirectory (jPath job))
            -- writemode, so a stale .part from a crash is overwritten
            n <- withBinaryFile part WriteMode $ \h ->
              let loop k = do
                    chunk <- brRead (responseBody res)
                    if B.null chunk
                      then pure k
                      else do
                        B.hPut h chunk
                        let k' = k + B.length chunk
                        atomicModifyIORef' bytes (\b -> (b + B.length chunk, ()))
                        loop k'
               in loop 0
            renameFile part (jPath job)
            pure (Right n)

-- output

-- sgr code around t, only on a terminal
colour :: Bool -> Text -> Text -> Text
colour tty code t = if tty then "\ESC[" <> code <> "m" <> t <> "\ESC[0m" else t

mb :: Int -> Text
mb n = T.pack (printf "%.1f" (fromIntegral n / 1e6 :: Double))

main :: IO ()
main = do
  mapM_ ($ utf8) [setLocaleEncoding, setFileSystemEncoding, setForeignEncoding]
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  hSetBuffering stdout LineBuffering
  (base, token) <- loadEnv
  tty <- hIsTerminalDevice stdout
  mgr <- newTlsManager
  entries <-
    starred mgr base token `catch` \e ->
      if isJust (fromException e :: Maybe ExitCode) then throwIO e else failWith ("miniflux: " <> T.pack (displayException (e :: SomeException)))
  ledger <- readLedger
  let withAudio = filter (any isAudio . eEnclosures) entries
      fresh = filter (not . (`S.member` ledger) . eId) withAudio
      planned = reverse . fst $ foldl (\(acc, taken) e -> let (ps, t') = entryPaths taken e in ((e, ps) : acc, t')) ([], S.empty) fresh
  checked <- forM planned $ \(e, ps) -> (,) e <$> mapM (\(enc, p) -> (,,) enc p <$> doesFileExist p) ps
  let present = [p | (_, ps) <- checked, (_, p, True) <- ps]
      jobs = [Job (eId e) (encUrl enc) p | (e, ps) <- checked, (enc, p, False) <- ps]
      count = T.pack . show . length
  TIO.putStrLn . colour tty "1" $
    "miniflux ▸ " <> count entries <> " starred · " <> count withAudio <> " with audio · "
      <> count jobs <> " files to fetch · " <> count present <> " present"
  mapM_ (TIO.putStrLn . colour tty "2" . ("• " <>) . T.pack) present
  -- everything already on disk, gpodder may have fetched it before
  forM_ [eId e | (e, ps) <- checked, all (\(_, _, here) -> here) ps] record
  if null jobs && null present
    then TIO.putStrLn (colour tty "2" "nothing new")
    else do
      -- per entry: files still to come, and whether all so far succeeded
      pending <- newMVar (M.fromListWith (\(a, _) (b, _) -> (a + b, True)) [(jEntry j, (1 :: Int, True)) | j <- jobs])
      bytes <- newIORef 0
      fetched <- newIORef (0 :: Int)
      out <- newMVar ()
      sem <- newQSem 4
      -- \r\ESC[K wipes the status line, which the next tick redraws
      let say t = withMVar out $ \_ -> TIO.putStr ((if tty then "\r\ESC[K" else "") <> t <> "\n")
          status = forever $ do
            b <- readIORef bytes
            k <- readIORef fetched
            withMVar out $ \_ -> do
              TIO.putStr ("\r\ESC[K↓ " <> T.pack (show k) <> "/" <> count jobs <> " files · " <> mb b <> " MB")
              hFlush stdout
            threadDelay 100000
          worker job = bracket_ (waitQSem sem) (signalQSem sem) $ do
            r <- download mgr bytes job
            atomicModifyIORef' fetched (\k -> (k + 1, ()))
            let ok = isRight r
            say $ case r of
              Right n -> colour tty "32" ("✓ " <> T.pack (jPath job) <> "  " <> mb n <> " MB")
              Left msg -> colour tty "31" ("✗ " <> T.pack (jPath job) <> "  " <> msg)
            modifyMVar_ pending $ \m -> do
              let (left, good) = M.findWithDefault (1, True) (jEntry job) m
                  st = (left - 1, good && ok)
              when (st == (0, True)) $ record (jEntry job)
              pure (M.insert (jEntry job) st m)
            pure ok
      results <- withAsync (when tty status) $ \_ -> mapConcurrently worker jobs
      when tty $ TIO.putStr "\r\ESC[K"
      let d = length (filter id results)
          f = length results - d
          num code k = colour tty code (T.pack (show k))
      TIO.putStrLn $
        "done: " <> num "32" d <> " downloaded, " <> num "2" (length present) <> " present, " <> num "31" f <> " failed"
      when (f > 0) $ exitWith (ExitFailure 1)
