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
  | not (started gstate)  = return gstate { cars = generateCars (level gstate) gstate, started = True }
  | status gstate == Lost = return (almostInitialState gstate)
  | otherwise             = return gstate { elapsedTime = elapsedTime gstate + secs, cars = moveCars gstate, status = frogTouchAnyCar gstate }
  
generateCars :: [Lane] -> GameState -> [[Car]]
generateCars (x:xs) gstate | null xs   = [[]]
                           | otherwise = generateCars' x gstate : generateCars xs gstate

--Puts the generated cars on the lane (right?)
generateCars' :: Lane -> GameState -> [Car]
generateCars' lane gstate = case lane of
                              NoCars    _ -> []
                              Finish    _ -> []
                              LeftSlow  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 1, CarL (-(randomFloatTo 560 gstate (y+211))) y 1, CarL (-(randomFloatTo 560 gstate (y+212))) y 1]
                              LeftFast  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 2, CarL (-(randomFloatTo 560 gstate (y+211))) y 2, CarL (-(randomFloatTo 560 gstate (y+212))) y 2]
                              RightSlow y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 1, CarR   (randomFloatTo 560 gstate (y+211))  y 1, CarR   (randomFloatTo 560 gstate (y+212))  y 1]
                              RightFast y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 2, CarR   (randomFloatTo 560 gstate (y+211))  y 2, CarR   (randomFloatTo 560 gstate (y+212))  y 2]

--Updates the state so the cars can move(?)
moveCars :: GameState -> [[Car]]
moveCars gstate = map (map (moveCar gstate)) (cars gstate)

--This function makes the cars move on the screen.
moveCar :: GameState -> Car -> Car
moveCar gstate (CarL x y s) | x > 255    = CarL (                         -300) y s
                            | anyCTC x y = CarL (-(randomFloatTo 560 gstate (x+560))) y s
                            | otherwise  = CarL (                        x + s) y s
                              where anyCTC x y = any (carTouchCar x y) (concat (cars gstate))
moveCar gstate (CarR x y s) | x < (-255) = CarR (                          300) y s
                            | anyCTC x y = CarR (  randomFloatTo 560 gstate (x+100) ) y s
                            | otherwise  = CarR (                        x - s) y s
                              where anyCTC x y = any (carTouchCar x y) (concat (cars gstate))
moveCar _      _            = Error

--Makes sure that cars do not touch each other, if they do (they get randomly placed somewhere else)
carTouchCar :: Float -> Float -> Car -> Bool
carTouchCar x1 y1 (CarL x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar x1 y1 (CarR x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar _  _   _           = False

--Checks the collision between the frog and car, if collision is detected the frog dies and has to start over.
frogTouchAnyCar :: GameState -> LevelStatus
frogTouchAnyCar gstate | any (frogTouchCar xpos ypos) (concat (cars gstate)) = Lost
                       | otherwise                                           = status gstate
                           where xpos = fst pos
                                 ypos = snd pos
                                 pos  = frog_pos gstate

--Checks if the frog touches a car, if yes ... the frog moves(? Wait was dis?)
frogTouchCar :: Float -> Float -> Car -> Bool
frogTouchCar xpos ypos (CarL x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar xpos ypos (CarR x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar _    _     _           = False

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

--These 2 functions deal with the frog (or Shrew) moving.
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

--This is a function for the camera that follows the frog throughout the game
calcCam :: Direction -> GameState -> Float
calcCam d gstate | ypos <= (-105)      = 0
                 | ypos >= ltop - 285  = camera gstate
                 | d == DDown          = ( -75) - ypos
                 | otherwise           = (-105) - ypos
                   where ypos = snd (frog_pos gstate)
                         ltop = (fromIntegral (length (level gstate))) * 30 - 210