{-# LANGUAGE RecordWildCards #-}

module Options (parseArgs) where

import           Commands            (MuCommand (..))
import           Config              (Config (Config, musicDir))
import           Control.Applicative (Alternative (..))
import           Lib                 (songsIn)
import           Options.Applicative (Parser, ParserInfo, argument, asum,
                                      completer, execParser, flag', fullDesc,
                                      header, help, helper, info,
                                      listIOCompleter, long, metavar, progDesc,
                                      short, str, strOption)

-- | Parse command line arguments into @MuCommand@
parseArgs :: Config -> IO MuCommand
parseArgs = execParser . opts

opts :: Config -> ParserInfo MuCommand
opts c = info (helper <*> commandParser c) $ mconcat [ fullDesc
                                                     , header "mu - a command line music player & downloader"
                                                     , progDesc "Play and manage songs"
                                                     ]

-- parsers

commandParser :: Config -> Parser MuCommand
commandParser c = asum [shuffleParser c, playParser c, updateParser, emptyParser]

emptyParser :: Parser MuCommand
emptyParser = pure (Shuffle [])

updateParser :: Parser MuCommand
updateParser = flag' Update $ mconcat [ long "update"
                                      , long "upgrade"
                                      , short 'u'
                                      , help "Perform an update"
                                      ]

playParser :: Config -> Parser MuCommand
playParser Config {..} = Play <$> some (argument str $ mconcat [ metavar "SONGS"
                                                               , completer (listIOCompleter (songsIn musicDir))
                                                               ])

shuffleParser :: Config -> Parser MuCommand
shuffleParser Config {..} = Shuffle <$> many (strOption $ mconcat [ long "shuffle"
                                                                  , metavar "[SONGS]"
                                                                  , completer (listIOCompleter (songsIn musicDir))
                                                                  , help "Shuffle through specified songs, or the entire library"
                                                                  ])
