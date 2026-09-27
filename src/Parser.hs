module Parser where
import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { romPath :: FilePath
  }

parse :: IO Options
parse = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo = do
  info (helper <*> optionsParser) $ fullDesc <> progDesc "A CHIP-8 Emulator thinked in Functional Programming." <> header "CHIP-8 Emulator"

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser

filePathParser :: Parser FilePath
filePathParser =
  argument str $ metavar "filePath" <> help "File Path of the game ROM to load."
