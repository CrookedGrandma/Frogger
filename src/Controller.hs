-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Types
import Scores
import Frog
import Shrew
import Car
import Movement

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | not (started gstate)        = return gstate { cars = generateCars (level gstate) gstate, highestY = snd (frog_pos gstate), started = True }
  | status gstate == InProgress = return (handleShrew secs (frogPNG secs ((stepGState { status = frogTouchAnyCar gstate }) { status = frogTouchShrew gstate })))
  | status gstate == Lost       = return (almostInitialState gstate) { loseScore = loseScore gstate + 10}
  | status gstate == Paused     = return gstate
  | status gstate == Won        = if not (savedScore gstate) then 
                                    do
                                    writeScores gstate
                                    return gstate { savedScore = True }
                                  else
                                    return gstate
  | otherwise                   = return (frogPNG secs stepGState)
    where stepGState = gstate { elapsedTime = elapsedTime gstate + secs, cars = moveCars gstate }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey k) Down _ _) gstate
  | status gstate == InProgress && k == KeyLeft  = (moveX Frog (-30) gstate) { frog_rot = 270 }
  | status gstate == InProgress && k == KeyRight = (moveX Frog   30  gstate) { frog_rot =  90 }
  | status gstate == InProgress && k == KeyUp    = (moveY Frog   30  gstate) { frog_rot =   0 }
  | status gstate == InProgress && k == KeyDown  = (moveY Frog (-30) gstate) { frog_rot = 180 }
  | otherwise                                    = gstate
inputKey (EventKey (Char k) Down _ _) gstate
  | k == 'p'                                     = pause gstate
  | k == 'h'                                     = pause gstate { highScreen = not (highScreen gstate)}
  | otherwise                                    = gstate
inputKey _ gstate                                = gstate

--Pause or unpause
pause :: GameState -> GameState
pause gstate | status gstate == InProgress = gstate { status = Paused }
             | status gstate == Paused     = gstate { status = InProgress }
             | otherwise                   = gstate