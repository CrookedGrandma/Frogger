module Movement where

import Model
import Types

--These 2 functions deal with the frog (or Shrew) moving.
moveX :: Walker -> Float -> GameState -> GameState
moveX w x gstate = case w of
                     Frog
                       | x < 0 && xfrog > (-225 - x) -> gstate { frog_pos = (xfrog + x, yfrog) }
                       | x > 0 && xfrog < ( 225 - x) -> gstate { frog_pos = (xfrog + x, yfrog) }
                       | otherwise                   -> gstate
                     Shrew
                       | x < 0 && xshrew > (-225 - x) -> gstate { shrew_pos = (xshrew + x, yshrew) }
                       | x > 0 && xshrew < ( 225 - x) -> gstate { shrew_pos = (xshrew + x, yshrew) }
                       | otherwise                    -> gstate
                     where xfrog  = fst fpos
                           yfrog  = snd fpos
                           fpos   = frog_pos gstate
                           xshrew = fst spos
                           yshrew = snd spos
                           spos   = shrew_pos gstate

moveY :: Walker -> Float -> GameState -> GameState
moveY w y gstate = case w of
                     Frog
                       | y < 0 && yfrog > (-210 - y)                                -> gstate { frog_pos = (xfrog, newy), camera = calcCam DDown gstate }
                       | y > 0 && yfrog < (ltop - 30 - y) && newy > highestY gstate -> gstate { frog_pos = (xfrog, newy), camera = calcCam   DUp gstate, score = score gstate + 10, highestY = newy }
                       | y > 0 && yfrog < (ltop - 30 - y)                           -> gstate { frog_pos = (xfrog, newy), camera = calcCam   DUp gstate }
                       | y > 0 && yfrog > (ltop - 30 - y)                           -> gstate { frog_pos = (xfrog, newy), camera = calcCam   DUp gstate, status = Won }
                       | otherwise                                                  -> gstate
                     Shrew
                       | y < 0 && yshrew > (-210 - y)      -> gstate { shrew_pos = (xshrew, yshrew + y) }
                       | y > 0 && yshrew < (ltop - 30 - y) -> gstate { shrew_pos = (xshrew, yshrew + y) }
                       | otherwise                         -> gstate
                     where xfrog  = fst fpos
                           yfrog  = snd fpos
                           fpos   = frog_pos gstate
                           ltop   = (fromIntegral (length (level gstate))) * 30 - 210
                           newy   = yfrog + y
                           xshrew = fst spos
                           yshrew = snd spos
                           spos   = shrew_pos gstate
                           
--This is a function for the camera that follows the frog throughout the game
calcCam :: Direction -> GameState -> Float
calcCam d gstate | yfrog <= (-105)      = 0
                 | yfrog >= ltop - 285  = camera gstate
                 | d == DDown           = ( -75) - yfrog
                 | otherwise            = (-105) - yfrog
                   where yfrog = snd (frog_pos gstate)
                         ltop  = (fromIntegral (length (level gstate))) * 30 - 210