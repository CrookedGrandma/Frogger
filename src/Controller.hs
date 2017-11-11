-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  -- | collision moet hier denk ik
  = -- Just update the elapsed time
    return gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey k) Down _ _) gstate
  | k == KeyLeft  = (moveX Frog (-30) gstate) { frog_rot = 270 }
  | k == KeyRight = (moveX Frog   30  gstate) { frog_rot =  90 }
  | k == KeyUp    = (moveY Frog   30  gstate) { frog_rot =   0 }
  | k == KeyDown  = (moveY Frog (-30) gstate) { frog_rot = 180 }
  | otherwise     = gstate
    where pos = frog_pos gstate
inputKey _ gstate = gstate

moveX :: Walker -> Float -> GameState -> GameState
moveX w x gstate = case w of
                     Frog
                       | x < 0 && xpos > (-225 - x) -> gstate { frog_pos = (xpos + x, ypos) }
                       | x > 0 && xpos < ( 225 - x) -> gstate { frog_pos = (xpos + x, ypos) }
                       | otherwise                  -> gstate
                     Shrew
                       -> gstate
                     where xpos = fst pos
                           ypos = snd pos
                           pos  = frog_pos gstate

moveY :: Walker -> Float -> GameState -> GameState
moveY w y gstate = case w of
                     Frog
                       | y < 0 && ypos > (-210 - y)      -> gstate { frog_pos = (xpos, ypos + y), camera = calcCam DDown gstate }
                       | y > 0 && ypos < (ltop - 30 - y) -> gstate { frog_pos = (xpos, ypos + y), camera = calcCam   DUp gstate }
                       | y > 0 && ypos > (ltop - 30 - y) -> gstate { frog_pos = (xpos, ypos + y), camera = calcCam   DUp gstate, status = Won }
                       | otherwise                       -> gstate
                     Shrew
                       -> gstate
                     where xpos = fst pos
                           ypos = snd pos
                           pos  = frog_pos gstate
                           ltop = (fromIntegral (length (level gstate))) * 30 - 210

calcCam :: Direction -> GameState -> Float
calcCam d gstate | ypos <= (-105)      = 0
                 | ypos >= ltop - 285  = camera gstate
                 | d == DDown          = ( -75) - ypos
                 | otherwise           = (-105) - ypos
                   where ypos = snd (frog_pos gstate)
                         ltop = (fromIntegral (length (level gstate))) * 30 - 210