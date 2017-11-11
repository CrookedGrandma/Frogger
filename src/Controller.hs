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
  | not (started gstate) = return gstate { cars = generateCars (level gstate) gstate, started = True }
  | otherwise            = return gstate { elapsedTime = elapsedTime gstate + secs, cars = moveCars (cars gstate) }
  
generateCars :: [Lane] -> GameState -> [[Car]]
generateCars (x:xs) gstate | null xs   = [[]]
                           | otherwise = generateCars' x gstate : generateCars xs gstate

generateCars' :: Lane -> GameState -> [Car]
generateCars' lane gstate = case lane of
                              NoCars    _ -> []
                              Finish    _ -> []
                              LeftSlow  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 1, CarL (-(randomFloatTo 560 gstate (y+211))) y 1, CarL (-(randomFloatTo 560 gstate (y+212))) y 1]
                              LeftFast  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 2, CarL (-(randomFloatTo 560 gstate (y+211))) y 2, CarL (-(randomFloatTo 560 gstate (y+212))) y 2]
                              RightSlow y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 1, CarR   (randomFloatTo 560 gstate (y+211))  y 1, CarR   (randomFloatTo 560 gstate (y+212))  y 1]
                              RightFast y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 2, CarR   (randomFloatTo 560 gstate (y+211))  y 2, CarR   (randomFloatTo 560 gstate (y+212))  y 2]

moveCars :: [[Car]] -> [[Car]]
moveCars c = map (map moveCar) c

moveCar :: Car -> Car
moveCar (CarL x y s) | x > 240    = CarL ( -300) y s
                     | otherwise  = CarL (x + s) y s
moveCar (CarR x y s) | x < (-240) = CarR    300  y s
                     | otherwise  = CarR (x - s) y s
moveCar _            = Error

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