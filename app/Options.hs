module Options (parseArgs) where

import           Commands            (MuCommand (..))
import           Control.Applicative (Alternative (..))
import           Options.Applicative (Parser, ParserInfo, argument, asum,
                                      execParser, flag', fullDesc, header, help,
                                      helper, info, long, metavar, progDesc,
                                      short, str)

-- | Parse command line arguments into @MuCommand@
parseArgs :: IO MuCommand
parseArgs = execParser opts

opts :: ParserInfo MuCommand
opts = info (helper <*> commandParser) $ mconcat [ fullDesc
                                                 , header "mu - a command line music player & downloader"
                                                 , progDesc "Play and manage songs"
                                                 ]

-- parsers

commandParser :: Parser MuCommand
commandParser = asum [shuffleParser, playParser, updateParser, emptyParser]

emptyParser :: Parser MuCommand
emptyParser = pure Shuffle

updateParser :: Parser MuCommand
updateParser = flag' Update $ mconcat [ long "update"
                                      , long "upgrade"
                                      , short 'u'
                                      , help "Perform an update"
                                      ]

playParser :: Parser MuCommand
playParser = Play <$> some (argument str (metavar "SONGS"))

shuffleParser :: Parser MuCommand
shuffleParser = flag' Shuffle $ mconcat [ long "shuffle"
                                        , help "Shuffle through all the somes in the library"
                                        ]

