module Frog where

import Model
import Types
import Car

import Graphics.Gloss
import Graphics.Gloss.Game

--Cycles through animation images
losingFrog :: GameState -> GameState
losingFrog gstate | loseImage gstate >    5 = gstate { status = Lost }
                  | loseTimer gstate >= 0.2 = gstate { frog_png = "frog_d" ++ show (loseImage gstate) ++ ".png", loseImage = loseImage gstate + 1, loseTimer = 0 }
                  | otherwise               = gstate
                    
--Checks the collision between the frog and car, if collision is detected the frog dies and has to start over.
frogTouchAnyCar :: GameState -> LevelStatus
frogTouchAnyCar gstate | status gstate == Losing                       = Losing
                       | any (frogTouchCar xpos ypos) (allCars gstate) = Losing
                       | otherwise                                     = status gstate
                         where xpos = fst pos
                               ypos = snd pos
                               pos  = frog_pos gstate

--Checks if the frog touches a car
frogTouchCar :: Float -> Float -> Car -> Bool
frogTouchCar xpos ypos (CarL x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar xpos ypos (CarR x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
frogTouchCar _    _     _           = False

--Checks if the frog touches the shrew
frogTouchShrew :: GameState -> LevelStatus
frogTouchShrew gstate | status gstate == Losing                                               = Losing
                      | (yfrog == yshrew) && (xfrog >= xshrew - 30) && (xfrog <= xshrew + 30) = Losing
                      | otherwise                                                             = status gstate
                        where xshrew = fst spos
                              yshrew = snd spos
                              spos   = shrew_pos gstate
                              xfrog  = fst fpos
                              yfrog  = snd fpos
                              fpos   = frog_pos gstate

--Returns the gamestate with a normal frog unless the player is losing, then returns a short animation
frogPNG :: Float -> GameState -> GameState
frogPNG secs gstate | status gstate == InProgress = gstate { frog_png = "frog.png" }
            | status gstate == Won        = gstate { frog_png = "frog.png" }
            | otherwise                   = losingFrog (gstate { loseTimer = loseTimer gstate + secs })

--Returns a picture of a frog on the position of the frog
frog :: GameState -> Picture
frog gstate = translate (fst pos) (snd pos) (rotate (frog_rot gstate) frogi)
  where pos   = frog_pos gstate
        frogi = png ("src/sprite/" ++ frog_png gstate)