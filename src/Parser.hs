module Parser where
import Data.Bits (shiftR)
import Data.Semigroup ((<>))
import Graphics (Color, Colors (..))
import Numeric (readHex)
import Options.Applicative
import Text.Printf (printf)

data Options = Options
  { romPath :: FilePath
  , windowSize :: (Int, Int)
  , colors :: Colors
  }

parse :: IO Options
parse = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo = do
  info (helper <*> optionsParser) $ fullDesc <> progDesc "A CHIP-8 Emulator thinked in Functional Programming." <> header "CHIP-8 Emulator"

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser <*> sizeParser <*> colorsParser

colorsParser :: Parser Colors
colorsParser =
  Colors
    <$> colorOption "foreground" (0xFF, 0xFF, 0xFF) "Color of lit pixels."
    <*> colorOption "background" (0x00, 0x00, 0x00) "Color of unlit pixels and borders."

colorOption :: String -> Color -> String -> Parser Color
colorOption name defaultColor description =
  option (eitherReader readColor) $
    long name <> metavar "RRGGBB" <> value defaultColor <> showDefaultWith showColor <> help description

readColor :: String -> Either String Color
readColor text = case dropWhile (== '#') text of
  hex
    | length hex == 6
    , [(rgb, "")] <- readHex hex ->
        Right (fromIntegral (rgb `shiftR` 16 :: Int), fromIntegral (rgb `shiftR` 8), fromIntegral rgb)
  _ -> Left $ "Invalid color " ++ show text ++ ", expected six hex digits like FF8800"

showColor :: Color -> String
showColor (red, green, blue) = printf "%02X%02X%02X" red green blue

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
