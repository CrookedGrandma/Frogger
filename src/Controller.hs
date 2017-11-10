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
  | k == KeyLeft  = (moveX (-30) gstate) { frog_rot = 270 }
  | k == KeyRight = (moveX   30  gstate) { frog_rot =  90 }
  | k == KeyUp    = (moveY   30  gstate) { frog_rot =   0 }
  | k == KeyDown  = (moveY (-30) gstate) { frog_rot = 180 }
  | otherwise     = gstate
    where pos = frog_pos gstate
inputKey _ gstate = gstate

moveX :: Float -> GameState -> GameState
moveX x gstate | x < 0 && xpos > (-225 - x) = gstate { frog_pos = (xpos + x, ypos) }
               | x > 0 && xpos < ( 225 - x) = gstate { frog_pos = (xpos + x, ypos) }
               | otherwise                  = gstate
                 where xpos = fst pos
                       ypos = snd pos
                       pos  = frog_pos gstate

moveY :: Float -> GameState -> GameState
moveY y gstate | y < 0 && ypos > (-200 - y) = gstate { frog_pos = (xpos, ypos + y) }
               | y > 0 && ypos < (ltop - y) = gstate { frog_pos = (xpos, ypos + y) }
               | otherwise                  = gstate
                 where xpos = fst pos
                       ypos = snd pos
                       pos  = frog_pos gstate
                       ltop = (fromIntegral (length (level gstate))) * 30 - 200