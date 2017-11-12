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

--Returns the top 5 scores in Picture format
pictureScores :: IO Picture
pictureScores = do
                rawScores <- readScores
                let firstScores = take 5 (lines rawScores)
                return (pictures (scores firstScores 90 1))

--Makes the strings into Pictures at the right scale and position
scores :: [String] -> Float -> Int -> [Picture]
scores [] _ _     = []
scores (x:xs) y p = translate (-100) y (scale 0.5 0.5 (text (show p ++ ". " ++ x))) : scores xs (y-69) (p+1)

--Returns the current score as picture
pictureScore :: GameState -> Picture
pictureScore gstate = translate (-200) (-200) (scale 0.3 0.3 (text (show (calcScore gstate))))