module Car where

import Model
import Types
import Graphics.Gloss
import Graphics.Gloss.Game

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
                            where anyCTC x y = any (carTouchCar x y) (allCars gstate)
moveCar gstate (CarR x y s) | x < (-255) = CarR (                              300)   y s
                            | anyCTC x y = CarR (  randomFloatTo 560 gstate (x+100) ) y s
                            | otherwise  = CarR (                            x - s)   y s
                            where anyCTC x y = any (carTouchCar x y) (allCars gstate)
moveCar _      _            = Error

--Checks if a car touches another car
carTouchCar :: Float -> Float -> Car -> Bool
carTouchCar x1 y1 (CarL x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar x1 y1 (CarR x y _) = (y1 == y) && (x1 >= x - 60) && (x1 <= x + 60) && (x1 /= x)
carTouchCar _  _   _           = False

--Produces a picture containing a picture for every car
viewCars :: GameState -> Picture
viewCars gstate = pictures (map viewCar (allCars gstate))

--Returns a picture of a car based its position and direction
viewCar :: Car -> Picture
viewCar (CarL x y _) = translate x y carL
                         where carL = png "src/sprite/car_l.png"
viewCar (CarR x y _) = translate x y carR
                         where carR = png "src/sprite/car_r.png"
viewCar _            = blank

--Returns a list of all cars
allCars :: GameState -> [Car]
allCars gstate = concat (cars gstate)