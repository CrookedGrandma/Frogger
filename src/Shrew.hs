module Shrew where

import Model
import Types
import Car
import Movement

import Graphics.Gloss
import Graphics.Gloss.Game

--Handle spawning and movement of the shrew
handleShrew :: Float -> GameState -> GameState
handleShrew secs gstate | shrewTimer gstate < 5                              = gstate { shrewTimer = shrewTimer gstate + secs }
                        | shrewTimer gstate > 5 && not (shrewSpawned gstate) = gstate { shrew_pos = (0, (-195)), shrewSpawned = True, shrewTimer = 10 }
                        | yshrew == yfrog                                    = if xfrog < xshrew then
                                                                                 shrewMove DLeft 15 gstate
                                                                               else
                                                                                 if xfrog > xshrew then
                                                                                   shrewMove DRight 15 gstate
                                                                                 else
                                                                                   gstate
                        | moveTimer gstate < 1                               = gstate { moveTimer = moveTimer gstate + secs}
                        | not (shrewTouchAnyCar xshrew (yshrew + 30) gstate) = shrewMove DUp 30 gstate
                        | lastMove gstate == DUp                             = if not (shrewTouchAnyCar (xshrew + 30) yshrew gstate) then
                                                                                 shrewMove DRight 30 gstate
                                                                               else
                                                                                 if not (shrewTouchAnyCar (xshrew - 30) yshrew gstate) then
                                                                                   shrewMove DLeft 30 gstate
                                                                                 else
                                                                                   shrewMove DUp 30 gstate
                        | lastMove gstate == DRight                          = if not (shrewTouchAnyCar (xshrew - 30) yshrew gstate) then
                                                                                 shrewMove DLeft 30 gstate
                                                                               else
                                                                                 shrewMove DUp 30 gstate
                        | lastMove gstate == DLeft                           = if not (shrewTouchAnyCar (xshrew + 30) yshrew gstate) then
                                                                                 shrewMove DRight 30 gstate
                                                                               else
                                                                                 shrewMove DUp 30 gstate
                        | otherwise                                          = gstate
                          where xshrew = fst spos
                                yshrew = snd spos
                                spos   = shrew_pos gstate
                                xfrog  = fst fpos
                                yfrog  = snd fpos
                                fpos   = frog_pos gstate

shrewMove :: Direction -> Float -> GameState -> GameState
shrewMove d x gstate = case d of
                         DUp    -> (moveY Shrew   x  gstate) { shrew_rot =   0, moveTimer = 0, lastMove = d }
                         DRight -> (moveX Shrew   x  gstate) { shrew_rot =  90, moveTimer = 0, lastMove = d }
                         DLeft  -> (moveX Shrew (-x) gstate) { shrew_rot = 270, moveTimer = 0, lastMove = d }

shrewTouchAnyCar :: Float -> Float -> GameState -> Bool
shrewTouchAnyCar x y gstate = any (shrewTouchCar x y) (allCars gstate)
                   
--Checks if the shrew touches a car
shrewTouchCar :: Float -> Float -> Car -> Bool
shrewTouchCar xpos ypos (CarL x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
shrewTouchCar xpos ypos (CarR x y _) = (ypos == y) && (xpos >= x - 45) && (xpos <= x + 45)
shrewTouchCar _    _     _           = False

shrew :: GameState -> Picture
shrew gstate = translate (fst pos) (snd pos) (rotate (shrew_rot gstate) shrewi)
  where pos    = shrew_pos gstate
        shrewi = png "src/sprite/shrew.png"