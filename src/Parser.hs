module Parser where
import Data.Bits (shiftR)
import Data.Semigroup ((<>))
import Graphics (Color, Colors (..))
import Keyboard (KeyMapping, keyMappingWith, readKeyBinding)
import Data.Char (toLower)
import Numeric (readHex)
import Options.Applicative
import Chip8.Interpreter (Interpreter, cosmac, interpreterProfiles)
import Text.Printf (printf)

data Options = Options
  { romPath :: FilePath
  , windowSize :: (Int, Int)
  , colors :: Colors
  , speed :: Int
  , keyMapping :: KeyMapping
  , tone :: Int
  , volume :: Int
  , interpreterProfile :: Interpreter
  }

parse :: IO Options
parse = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo = do
  info (helper <*> optionsParser) $ fullDesc <> progDesc "A CHIP-8 Emulator thinked in Functional Programming." <> header "CHIP-8 Emulator"

optionsParser :: Parser Options
optionsParser =
  Options <$> filePathParser <*> sizeParser <*> colorsParser <*> speedParser <*> keyMappingParser
    <*> toneParser <*> volumeParser <*> interpreterParser

interpreterParser :: Parser Interpreter
interpreterParser =
  option (eitherReader readInterpreter) $
    long "interpreter" <> metavar "NAME" <> value cosmac <> showDefaultWith (const "cosmac")
      <> help "Original CHIP-8 interpreter to behave like: cosmac (COSMAC VIP) or superchip (SUPER-CHIP 1.1)."

readInterpreter :: String -> Either String Interpreter
readInterpreter text = case lookup (map toLower text) interpreterProfiles of
  Just profile -> Right profile
  Nothing -> Left $ "Invalid interpreter " ++ show text ++ ", expected cosmac or superchip"

toneParser :: Parser Int
toneParser =
  option (eitherReader (readIntBetween "tone" 20 20000)) $
    long "tone" <> metavar "HZ" <> value 440 <> showDefault
      <> help "Pitch of the beep, from 20 to 20000 Hz."

volumeParser :: Parser Int
volumeParser =
  option (eitherReader (readIntBetween "volume" 0 100)) $
    long "volume" <> metavar "0-100" <> value 10 <> showDefault
      <> help "Loudness of the beep. 0 turns sound off."

readIntBetween :: String -> Int -> Int -> String -> Either String Int
readIntBetween name lowest highest text = case reads text of
  [(number, "")] | number >= lowest && number <= highest -> Right number
  _ -> Left $ "Invalid " ++ name ++ " " ++ show text ++ ", expected a whole number from "
         ++ show lowest ++ " to " ++ show highest

keyMappingParser :: Parser KeyMapping
keyMappingParser =
  fmap keyMappingWith . many $
    option (eitherReader readKeyBinding) $
      long "key" <> metavar "NAME=HEX"
        <> help "Make keyboard key NAME press CHIP-8 key HEX, e.g. Left=4. Can be repeated. The default keys keep working."

-- The CHIP-8 has no official clock speed; about 600 instructions per
-- second runs most games at their intended pace.
speedParser :: Parser Int
speedParser =
  option (eitherReader readSpeed) $
    long "speed" <> metavar "N" <> value 600 <> showDefault
      <> help "Instructions executed per second."

readSpeed :: String -> Either String Int
readSpeed text = case reads text of
  [(instructions, "")] | instructions >= 1 -> Right instructions
  _ -> Left $ "Invalid speed " ++ show text ++ ", expected a whole number of at least 1"

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
      <> help "Initial window size, a multiple of 64x32. The window can be resized while playing."

-- Only exact multiples of 64x32, so every CHIP-8 pixel is the same size
-- and the screen fills the window.
readSize :: String -> Either String (Int, Int)
readSize text = case break (== 'x') text of
  (widthText, 'x' : heightText)
    | [(width, "")] <- reads widthText
    , [(height, "")] <- reads heightText ->
        if width >= 64 && width `mod` 64 == 0 && height * 2 == width
          then Right (width, height)
          else Left $ "Invalid size " ++ show text ++ ", expected a multiple of 64x32 such as "
                 ++ suggestions width
  _ -> Left $ "Invalid size " ++ show text ++ ", expected WIDTHxHEIGHT such as 1024x512"
  where
    suggestions width = case filter (>= 1) [width `div` 64, width `div` 64 + 1] of
      [smaller, larger] | smaller /= larger -> showSize (sizeFor smaller) ++ " or " ++ showSize (sizeFor larger)
      multiples -> showSize (sizeFor (last multiples))
    sizeFor multiple = (64 * multiple, 32 * multiple)

showSize :: (Int, Int) -> String
showSize (width, height) = show width ++ "x" ++ show height

filePathParser :: Parser FilePath
filePathParser =
  argument str $ metavar "filePath" <> help "File Path of the game ROM to load."
