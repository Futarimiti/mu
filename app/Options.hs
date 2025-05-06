module Options (parseArgs) where

import Commands               (MuCommand (..))
import Config                 (Config (..))
import Control.Applicative    (Alternative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader   (MonadReader, asks)
import Global                 (Global (..))
import Lib                    (songsIn)
import Options.Applicative    (Parser, ParserInfo, argument, asum, completer, execParser,
                               flag', fullDesc, header, help, helper, info, listCompleter,
                               long, metavar, progDesc, short, str, strOption)

parseArgs :: (MonadIO m, MonadReader Global m) => m MuCommand
parseArgs = optsT >>= (liftIO . execParser)

optsT :: (MonadIO m, MonadReader Global m) => m (ParserInfo MuCommand)
optsT = do
  parser <- commandParserT
  pure $ info (helper <*> parser) $ mconcat
    [ fullDesc
    , header "mu - a command line music player & downloader"
    , progDesc "Play and manage songs"
    ]

-- parsers

commandParserT :: (MonadIO m, MonadReader Global m) => m (Parser MuCommand)
commandParserT = do
  play <- playParserT
  shuffle <- shuffleParserT
  pure $ asum [play, shuffle, updateParser, emptyParser]

emptyParser :: Parser MuCommand
emptyParser = pure (Shuffle [])

updateParser :: Parser MuCommand
updateParser = flag' Update $ mconcat
  [ long "update"
  , long "upgrade"
  , short 'u'
  , help "Perform an update"
  ]

playParserT :: (MonadIO m, MonadReader Global m) => m (Parser MuCommand)
playParserT = do
  mdir <- asks (musicDir . config)
  songs <- songsIn mdir
  pure $ Play <$> some (argument str $ mconcat
    [ metavar "SONGS"
    , completer (listCompleter songs)
    ])

shuffleParserT :: (MonadIO m, MonadReader Global m) => m (Parser MuCommand)
shuffleParserT = do
  mdir <- asks (musicDir . config)
  songs <- songsIn mdir
  pure $ Shuffle <$> many (strOption $ mconcat
    [ long "shuffle"
    , metavar "[SONGS]"
    , completer (listCompleter songs)
    , help "Shuffle through specified songs, or the entire library"
    ])
