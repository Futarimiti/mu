module Options (parseArgs) where

import           Commands                   (MuCommand (..))
import           Config                     (Config (..))
import           Control.Applicative        (Alternative (..))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Lib                        (songsIn)
import           Options.Applicative        (Parser, ParserInfo, argument, asum,
                                             completer, execParser, flag',
                                             fullDesc, header, help, helper,
                                             info, listIOCompleter, long,
                                             metavar, progDesc, short, str,
                                             strOption)

parseArgs :: ReaderT Config IO MuCommand
parseArgs = optsT >>= lift . execParser

optsT :: Monad m => ReaderT Config m (ParserInfo MuCommand)
optsT = do parser <- commandParserT
           return $ info (helper <*> parser) $ mconcat [ fullDesc
                                                       , header "mu - a command line music player & downloader"
                                                       , progDesc "Play and manage songs"
                                                       ]

-- parsers

commandParserT :: Monad m => ReaderT Config m (Parser MuCommand)
commandParserT = do play <- playParserT
                    shuffle <- shuffleParserT
                    return $ asum [play, shuffle, updateParser, emptyParser]

emptyParser :: Parser MuCommand
emptyParser = pure (Shuffle [])

updateParser :: Parser MuCommand
updateParser = flag' Update $ mconcat [ long "update"
                                      , long "upgrade"
                                      , short 'u'
                                      , help "Perform an update"
                                      ]

playParserT :: Monad m => ReaderT Config m (Parser MuCommand)
playParserT = do c <- ask
                 pure $ Play <$> some (argument str $ mconcat [ metavar "SONGS"
                                                              , completer (listIOCompleter (songsIn (musicDir c)))
                                                              ])

shuffleParserT :: Monad m => ReaderT Config m (Parser MuCommand)
shuffleParserT = do c <- ask
                    pure $ Shuffle <$> many (strOption $ mconcat [ long "shuffle"
                                                                 , metavar "[SONGS]"
                                                                 , completer (listIOCompleter (songsIn (musicDir c)))
                                                                 , help "Shuffle through specified songs, or the entire library"
                                                                 ])
