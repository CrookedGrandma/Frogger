-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Directory
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | not (started gstate)        = return gstate { cars = generateCars (level gstate) gstate, highestY = snd (frog_pos gstate), started = True }
  | status gstate == InProgress = return (frogPNG secs (stepGState { status = frogTouchAnyCar gstate }))
  | status gstate == Lost       = return (almostInitialState gstate) { loseScore = loseScore gstate + 10}
  | status gstate == Paused     = return gstate
  | status gstate == Won        = if not (savedScore gstate) then 
                                    do
                                    oldscores <- readFile "src/score/score.txt"
                                    writeFile "src/score/newscore.txt" (newScores gstate oldscores)
                                    removeFile "src/score/score.txt"
                                    renameFile "src/score/newscore.txt" "src/score/score.txt"
                                    return gstate { savedScore = True }
                                  else
                                    return gstate
  | otherwise                   = return (frogPNG secs stepGState)
    where stepGState = gstate { elapsedTime = elapsedTime gstate + secs, cars = moveCars gstate }

--Determines what to write in the list of scores
newScores :: GameState -> String -> String
newScores gstate s = unlines (map show (sortBy (flip compare) (yourScore : map convert (lines s))))
                     where yourScore = calcScore gstate

--Convert string to int
convert :: String -> Int
convert s = let i = read s :: Int in i

--Calculates the end score based on amount of lanes, amount of fails, and elapsed time
calcScore :: GameState -> Int
calcScore gstate = score gstate - loseScore gstate - floor (elapsedTime gstate) * 2

--Returns the gamestate with a normal frog unless the player is losing, then returns a short animation
frogPNG :: Float -> GameState -> GameState
frogPNG secs gstate | status gstate == InProgress = gstate { frog_png = "frog.png" }
                    | status gstate == Won        = gstate { frog_png = "frog.png" }
                    | otherwise                   = losingFrog (gstate { loseTimer = loseTimer gstate + secs })

--Cycles through animation images
losingFrog :: GameState -> GameState
losingFrog gstate | loseImage gstate >    5 = gstate { status = Lost }
                  | loseTimer gstate >= 0.2 = gstate { frog_png = "frog_d" ++ show (loseImage gstate) ++ ".png", loseImage = loseImage gstate + 1, loseTimer = 0 }
                  | otherwise               = gstate
  
--Produce a list of cars for every lane
generateCars :: [Lane] -> GameState -> [[Car]]
generateCars (x:xs) gstate | null xs   = [[]]
                           | otherwise = generateCars' x gstate : generateCars xs gstate

--Puts the generated cars on the lane
generateCars' :: Lane -> GameState -> [Car]
generateCars' lane gstate = case lane of
                              NoCars    _ -> []
                              Finish    _ -> []
                              LeftSlow  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 1, CarL (-(randomFloatTo 560 gstate (y+211))) y 1, CarL (-(randomFloatTo 560 gstate (y+212))) y 1]
                              LeftFast  y -> [CarL (-(randomFloatTo 560 gstate (y+210))) y 2, CarL (-(randomFloatTo 560 gstate (y+211))) y 2, CarL (-(randomFloatTo 560 gstate (y+212))) y 2]
                              RightSlow y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 1, CarR   (randomFloatTo 560 gstate (y+211))  y 1, CarR   (randomFloatTo 560 gstate (y+212))  y 1]
                              RightFast y -> [CarR   (randomFloatTo 560 gstate (y+210))  y 2, CarR   (randomFloatTo 560 gstate (y+211))  y 2, CarR   (randomFloatTo 560 gstate (y+212))  y 2]

--Moves all cars in the GameState
moveCars :: GameState -> [[Car]]
moveCars gstate = map (map (moveCar gstate)) (cars gstate)

--Move a car depending on if they're out of the screen and if they collide with another car (if they do, they get randomly placed somewhere else)
moveCar :: GameState -> Car -> Car
moveCar gstate (CarL x y s) | x > 255    = CarL (                             -300)   y s
                            | anyCTC x y = CarL (-(randomFloatTo 560 gstate (x+560))) y s
                            | otherwise  = CarL (                            x + s)   y s
                              where anyCTC x y = any (carTouchCar x y) (concat (cars gstate))
moveCar gstate (CarR x y s) | x < (-255) = CarR (                              300)   y s
                            | anyCTC x y = CarR (  randomFloatTo 560 gstate (x+100) ) y s
                            | otherwise  = CarR (                            x - s)   y s
                              where anyCTC x y = any (carTouchCar x y) (concat (cars gstate))
moveCar _      _            = Error

--Checks if a car touches another car
carTouchCar :: Float -> Float -> Car -> Bool
carTouchCar x1 y1 (CarL x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar x1 y1 (CarR x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar _  _   _           = False

--Checks the collision between the frog and car, if collision is detected the frog dies and has to start over.
frogTouchAnyCar :: GameState -> LevelStatus
frogTouchAnyCar gstate | status gstate == Losing                             = Losing
                       | any (frogTouchCar xpos ypos) (concat (cars gstate)) = Losing
                       | otherwise                                           = status gstate
                           where xpos = fst pos
                                 ypos = snd pos
                                 pos  = frog_pos gstate

--Checks if the frog touches a car
frogTouchCar :: Float -> Float -> Car -> Bool
frogTouchCar xpos ypos (CarL x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar xpos ypos (CarR x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar _    _     _           = False

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
  | otherwise                                    = gstate
inputKey _ gstate                                = gstate

--Pause or unpause
pause :: GameState -> GameState
pause gstate | status gstate == InProgress = gstate { status = Paused }
             | status gstate == Paused     = gstate { status = InProgress }
             | otherwise                   = gstate

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
                       | y < 0 && ypos > (-210 - y)                                -> gstate { frog_pos = (xpos, newy), camera = calcCam DDown gstate }
                       | y > 0 && ypos < (ltop - 30 - y) && newy > highestY gstate -> gstate { frog_pos = (xpos, newy), camera = calcCam   DUp gstate, score = score gstate + 10, highestY = newy }
                       | y > 0 && ypos < (ltop - 30 - y)                           -> gstate { frog_pos = (xpos, newy), camera = calcCam   DUp gstate }
                       | y > 0 && ypos > (ltop - 30 - y)                           -> gstate { frog_pos = (xpos, newy), camera = calcCam   DUp gstate, status = Won }
                       | otherwise                                                 -> gstate
                     Shrew
                       -> gstate
                     where xpos = fst pos
                           ypos = snd pos
                           pos  = frog_pos gstate
                           ltop = (fromIntegral (length (level gstate))) * 30 - 210
                           newy = ypos + y

--This is a function for the camera that follows the frog throughout the game
calcCam :: Direction -> GameState -> Float
calcCam d gstate | ypos <= (-105)      = 0
                 | ypos >= ltop - 285  = camera gstate
                 | d == DDown          = ( -75) - ypos
                 | otherwise           = (-105) - ypos
                   where ypos = snd (frog_pos gstate)
                         ltop = (fromIntegral (length (level gstate))) * 30 - 210