-- | This module defines how to turn
--   the game state into a picture
module View where

import Model
import Types
import Scores
import Frog
import Car
import Shrew

import Graphics.Gloss
import Graphics.Gloss.Game

view :: GameState -> IO Picture
view gstate | highScreen gstate = do
                                  let gameScreen = viewPure gstate
                                  scores <- pictureScores
                                  return (pictures (gameScreen : [scores]))
            | otherwise         = return (pictures (viewPure gstate : [pictureScore gstate]))

--All the things that are eventually put on the screen
viewPure :: GameState -> Picture
viewPure gstate | highScreen gstate       = highScoreScreen
                | status gstate == Intro  = introScreen
                | status gstate == Won    = winScreen
                | status gstate == Paused = pictures (gameV : [pauseScreen])
                | otherwise               = gameV
                  where gameV = translate 0 (camera gstate) (pictures (lanes (level gstate) ++ [shrew gstate] ++ [viewCars gstate] ++ [frog gstate]))

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


--Just pictures
introScreen :: Picture
introScreen = png "src/sprite/intro.png"

winScreen :: Picture
winScreen = png "src/sprite/win.png"

pauseScreen :: Picture
pauseScreen = png "src/sprite/pause.png"

highScoreScreen :: Picture
highScoreScreen = png "src/sprite/high.png"