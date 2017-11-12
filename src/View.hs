-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Game
import Model
import Types

view :: GameState -> IO Picture
view = return . viewPure

--All the things that are eventually put on the screen
viewPure :: GameState -> Picture
viewPure gstate | status gstate == Won = winScreen
                | otherwise            = translate 0 (camera gstate) (pictures (lanes (level gstate) ++ [viewCars gstate] ++ [frog gstate]))

--Returns a picture of a frog on the position of the frog
frog :: GameState -> Picture
frog gstate = translate (fst pos) (snd pos) (rotate (frog_rot gstate) frogi)
  where pos   = frog_pos gstate
        frogi = png "src/sprite/frog_small.png"

--Returns a list of pictures of different lanes
lanes :: [Lane] -> [Picture]
lanes []     = []
lanes (x:xs) = case x of
                 NoCars    a -> translate 0 a (png "src/sprite/restlane.png")   : lanes xs
                 LeftSlow  a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 LeftFast  a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 RightSlow a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 RightFast a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 Finish    a -> translate 0 a (png "src/sprite/finishlane.png") : lanes xs


--Just a picture
winScreen :: Picture
winScreen = png "src/sprite/win.png"

--Produces a picture containing a picture for every car
viewCars :: GameState -> Picture
viewCars gstate = pictures (map viewCar (concat (cars gstate)))

--Returns a picture of a car based its position and direction
viewCar :: Car -> Picture
viewCar (CarL x y _) = translate x y carL
viewCar (CarR x y _) = translate x y carR
viewCar _            = blank

--Just pictures
carL :: Picture
carL = png "src/sprite/car_l.png"

carR :: Picture
carR = png "src/sprite/car_r.png"