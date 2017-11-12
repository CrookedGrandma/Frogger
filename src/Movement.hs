module Movement where

import Model
import Types

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