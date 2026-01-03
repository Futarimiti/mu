module Commandline
  ( Command (..)
  , Options (..)
  , PlayOrder (..)
  , SomeOrAll
  , SomeWithOptsOrAll (..)
  , parseArgs
  ) where

import Control.Monad.IO.Class
import Data.List.NonEmpty
import Data.Maybe
import Options.Applicative
import Options.Applicative    qualified as Optparse
import Types

parseArgs :: (MonadIO m) => m Options
parseArgs = liftIO (execParser opts)

opts :: ParserInfo Options
opts = info (helper <*> parser) $ mconcat
  [ fullDesc
  , header "mu - a command line music player & downloader"
  , progDesc "Play and manage tracks"
  ]

parser :: Parser Options
parser = do
  command <- fromMaybe (PlaySome All) <$> optional pcommand
  verbose <- pverbose
  confFile <- optional pconfFile
  pure Options {..}

pverbose :: Parser Bool
pverbose = switch $ mconcat
  [ long "verbose"
  , short 'V'
  , help "Print logs"
  ]

pcommand, pplaySomeFallback, psubcommand :: Parser Command
pcommand = psubcommand <|> pplaySomeFallback
pplaySomeFallback = PlaySome <$> pgiven "TRACK" pplayOrder
psubcommand = hsubparser $ mconcat
  [ playSomeMod
  , playListMod
  , removeDownloadMod
  , ensureDownloadMod
  , listSomeMod
  ]

removeDownloadMod, playListMod, playSomeMod, ensureDownloadMod, listSomeMod
  :: Mod CommandFields Command
removeDownloadMod = Optparse.command "remove" piremoveDownload
playListMod = Optparse.command "playlist" piplayList
playSomeMod = Optparse.command "play" piplaySome
ensureDownloadMod = Optparse.command "ensure-download" piensureDownload
listSomeMod = Optparse.command "list" pilistSome

piensureDownload, piremoveDownload, piplayList, piplaySome, pilistSome
  :: ParserInfo Command
piensureDownload = info pensureDownload (progDesc "Ensure track downloads")
piremoveDownload = info premoveDownload (progDesc "Remove track downloads")
piplayList = info pplayList (progDesc "Play through a playlist")
piplaySome = info pplaySome (progDesc "Play one or more tracks")
pilistSome = info plistSome (progDesc "List library or playlist(s)")

pensureDownload, premoveDownload, pplayList, pplaySome, plistSome
  :: Parser Command
pensureDownload = EnsureDownload <$> ptracks
premoveDownload = RemoveDownload <$> ptracks
pplayList = liftA2 PlayList pplayOrder pplayList
  where
    pplayList :: Parser String
    pplayList = strArgument $ metavar "PLAYLIST"
pplaySome = PlaySome <$> ptracksWithOrder
plistSome = ListSome <$> pplaylists

pplaylists :: Parser (SomeOrAll String)
pplaylists = do
  m <- optional $ pgiven' <|> pall
  pure $ fromMaybe All m
  where pgiven' = pgiven "PLAYLIST" (pure ())

ptracksWithOrder :: Parser (SomeWithOptsOrAll PlayOrder String)
ptracksWithOrder = pgiven "TRACK" pplayOrder <|> pall

pgiven :: String -> Parser a -> Parser (SomeWithOptsOrAll a String)
pgiven metavarName pa = liftA2 Given pa (some1 . strArgument $ metavar metavarName)

ptracks :: Parser (SomeOrAll String)
ptracks = pgiven' <|> pall
  where pgiven' = pgiven "TRACK" (pure ())

pall :: Parser (SomeWithOptsOrAll a String)
pall = flag' All $ long "all" <> help "Shuffle through all"

pplayOrder :: Parser PlayOrder
pplayOrder = flag Sequential Shuffle $ mconcat
  [ long "shuffle"
  , long "random"
  , short 'r'
  , help "Play in random order (sequential by default)"
  ]

pconfFile :: Parser FilePath
pconfFile = strOption $ mconcat
  [ long "config"
  , short 'c'
  , metavar "FILE"
  , help "Path to user config file"
  ]
