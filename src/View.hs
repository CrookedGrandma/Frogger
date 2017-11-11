-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Game
import Model
import Types

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate | status gstate == Won = winScreen
                | otherwise            = translate 0 (camera gstate) (pictures (lanes (level gstate) ++ [frog gstate]))

frog :: GameState -> Picture
frog gstate = translate (fst pos) (snd pos) (rotate (frog_rot gstate) frog)
  where pos  = frog_pos gstate
        frog = png "src/sprite/frog_small.png"

lanes :: [Lane] -> [Picture]
lanes []     = []
lanes (x:xs) = case x of
                 NoCars    a -> translate 0 a (png "src/sprite/restlane.png")   : lanes xs
                 LeftSlow  a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 LeftFast  a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 RightSlow a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 RightFast a -> translate 0 a (png "src/sprite/lane.png")       : lanes xs
                 Finish    a -> translate 0 a (png "src/sprite/finishlane.png") : lanes xs


winScreen :: Picture
winScreen = png "src/sprite/win.png"