module Parser where
import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { romPath :: FilePath
  , windowSize :: (Int, Int)
  }

parse :: IO Options
parse = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo = do
  info (helper <*> optionsParser) $ fullDesc <> progDesc "A CHIP-8 Emulator thinked in Functional Programming." <> header "CHIP-8 Emulator"

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser <*> sizeParser

sizeParser :: Parser (Int, Int)
sizeParser =
  option (eitherReader readSize) $
    long "size" <> metavar "WIDTHxHEIGHT" <> value (1024, 512) <> showDefaultWith showSize
      <> help "Initial window size. The window can be resized while playing."

readSize :: String -> Either String (Int, Int)
readSize text = case break (== 'x') text of
  (widthText, 'x' : heightText)
    | [(width, "")] <- reads widthText
    , [(height, "")] <- reads heightText
    , width >= 64
    , height >= 32 -> Right (width, height)
  _ -> Left $ "Invalid size " ++ show text ++ ", expected WIDTHxHEIGHT of at least 64x32"

showSize :: (Int, Int) -> String
showSize (width, height) = show width ++ "x" ++ show height

filePathParser :: Parser FilePath
filePathParser =
  argument str $ metavar "filePath" <> help "File Path of the game ROM to load."
