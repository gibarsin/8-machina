module Chip8.Interpreter where

-- Behaviours that differ between the original CHIP-8 interpreters.
data Interpreter = Interpreter
  { shiftUsesVy :: Bool           -- 8xy6 and 8xyE shift Vy into Vx, instead of shifting Vx in place
  , loadStoreIncrementsI :: Bool  -- Fx55 and Fx65 leave I after the last register used
  , jumpUsesV0 :: Bool            -- Bnnn adds V0, instead of Vx where x is the top digit of nnn
  , clipSprites :: Bool           -- sprite pixels past the screen edge are cut off instead of wrapping
  , logicResetsVF :: Bool         -- 8xy1, 8xy2 and 8xy3 set VF to 0
  }

-- The original COSMAC VIP interpreter (1977).
cosmac :: Interpreter
cosmac = Interpreter
  { shiftUsesVy = True
  , loadStoreIncrementsI = True
  , jumpUsesV0 = True
  , clipSprites = True
  , logicResetsVF = True
  }

-- SUPER-CHIP 1.1 on the HP 48 calculators.
superchip :: Interpreter
superchip = Interpreter
  { shiftUsesVy = False
  , loadStoreIncrementsI = False
  , jumpUsesV0 = False
  , clipSprites = True
  , logicResetsVF = False
  }

interpreterProfiles :: [(String, Interpreter)]
interpreterProfiles = [("cosmac", cosmac), ("superchip", superchip)]
