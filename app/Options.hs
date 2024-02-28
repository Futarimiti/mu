module Options (parseArgs) where

import           Commands                  (MuCommand (..))
import           Config                    (Config (..))
import           Control.Applicative       (Alternative (..))
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (ReaderT, asks, withReaderT)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Global                    (Global (config, fileinfo))
import           Lib                       (songsIn)
import           Options.Applicative       (Parser, ParserInfo, argument, asum,
                                            completer, execParser, flag',
                                            fullDesc, header, help, helper,
                                            info, listCompleter, long, metavar,
                                            progDesc, short, str, strOption)

parseArgs :: ReaderT Global IO MuCommand
parseArgs = optsT >>= lift . execParser

optsT :: MonadIO m => ReaderT Global m (ParserInfo MuCommand)
optsT = do parser <- commandParserT
           return $ info (helper <*> parser) $ mconcat
             [ fullDesc
             , header "mu - a command line music player & downloader"
             , progDesc "Play and manage songs"
             ]

-- parsers

commandParserT :: MonadIO m => ReaderT Global m (Parser MuCommand)
commandParserT = do play <- playParserT
                    shuffle <- shuffleParserT
                    return $ asum [play, shuffle, updateParser, emptyParser]

emptyParser :: Parser MuCommand
emptyParser = pure (Shuffle [])

updateParser :: Parser MuCommand
updateParser = flag' Update $ mconcat
  [ long "update"
  , long "upgrade"
  , short 'u'
  , help "Perform an update"
  ]

playParserT :: MonadIO m => ReaderT Global m (Parser MuCommand)
playParserT = do mdir <- asks (musicDir . config)
                 songs <- withReaderT fileinfo (songsIn mdir)
                 pure $ Play <$> some (argument str $ mconcat
                   [ metavar "SONGS"
                   , completer (listCompleter songs)
                   ])

shuffleParserT :: MonadIO m => ReaderT Global m (Parser MuCommand)
shuffleParserT = do mdir <- asks (musicDir . config)
                    songs <- withReaderT fileinfo (songsIn mdir)
                    pure $ Shuffle <$> many (strOption $ mconcat
                      [ long "shuffle"
                      , metavar "[SONGS]"
                      , completer (listCompleter songs)
                      , help "Shuffle through specified songs, or the entire library"
                      ])
