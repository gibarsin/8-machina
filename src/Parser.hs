module Parser where
import Data.Semigroup ((<>))
import Options.Applicative

parse :: IO FilePath
parse = execParser parserInfo

parserInfo :: ParserInfo FilePath
parserInfo = do
  info (helper <*> filePathParser) $ fullDesc <> progDesc "A CHIP-8 Emulator thinked in Functional Programming." <> header "CHIP-8 Emulator"

filePathParser :: Parser FilePath
filePathParser =
  argument str $ metavar "filePath" <> help "File Path of the game ROM to load."
