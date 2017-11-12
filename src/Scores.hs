module Scores where

import Model
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Game
import System.Directory

--Determines what to write in the list of scores
newScores :: GameState -> String -> String
newScores gstate s = unlines (map show (sortBy (flip compare) (yourScore : map convert (lines s))))
                     where yourScore = calcScore gstate

--Calculates the end score based on amount of lanes, amount of fails, and elapsed time
calcScore :: GameState -> Int
calcScore gstate = score gstate - loseScore gstate - floor (elapsedTime gstate) * 2

--Read scores
readScores :: IO String
readScores = readFile "src/score/score.txt"

--Write scores
writeScores :: GameState -> IO ()
writeScores gstate = do 
                     oldscores <- readScores
                     writeFile "src/score/newscore.txt" (newScores gstate oldscores)
                     removeFile "src/score/score.txt"
                     renameFile "src/score/newscore.txt" "src/score/score.txt"

pictureScores :: IO Picture
pictureScores = return (png "src/sprite/frog.png")