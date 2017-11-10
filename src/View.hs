-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Game
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = translate (fst pos) (snd pos) (rotate (frog_rot gstate) frog)
    where pos = frog_pos gstate

frog :: Picture
frog = png "src/sprite/frog_small.png"