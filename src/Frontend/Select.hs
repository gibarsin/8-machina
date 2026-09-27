module Frontend.Select (open) where

import Frontend (Frontend, FrontendKind (..))
import qualified Frontend.Brick as BrickFrontend
import qualified Frontend.SDL as SDLFrontend
import Parser (Options (..))

-- The only place that knows every concrete Frontend implementation exists.
open :: Options -> IO Frontend
open options = case frontendKind options of
  UseSdlFrontend -> SDLFrontend.open options
  UseTerminalFrontend -> BrickFrontend.open options
