-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- | loopanimatie
  = -- Just update the elapsed time
    return gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey k) Down _ _) gstate
  | k == KeyLeft  = gstate { frog_pos = (fst pos - 30, snd pos), frog_rot = 270 }
  | k == KeyRight = gstate { frog_pos = (fst pos + 30, snd pos), frog_rot =  90 }
  | k == KeyUp    = gstate { frog_pos = (fst pos, snd pos + 30), frog_rot =   0 }
  | k == KeyDown  = gstate { frog_pos = (fst pos, snd pos - 30), frog_rot = 180 }
  | otherwise     = gstate
    where pos = frog_pos gstate
inputKey _ gstate = gstate

moveX :: Float -> GameState -> GameState
moveX = undefined