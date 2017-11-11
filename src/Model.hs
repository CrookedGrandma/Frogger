-- | This module contains the data types
--   which represent the state of the game
module Model where

import Parser
import Types
import System.Random

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   elapsedTime :: Float
                 , frog_pos    :: (Float, Float)
                 , frog_rot    :: Float
                 , level       :: [Lane]
                 , status      :: LevelStatus
                 , camera      :: Float
                 , cars        :: [[Car]]
                 , rands       :: [Float]
                 }

initialState :: GameState
initialState = GameState 0 (0, (-195)) 0 (parseLevel "nrRrRnlLlLnRLlnlnlnlnlnf") InProgress 0 [[]] (randoms (mkStdGen 1))

randomIntTo :: Int -> GameState -> Int
randomIntTo x gstate = ceiling (head (rands gstate) * fromIntegral x)

randomFloatTo :: Float -> GameState -> Float
randomFloatTo x gstate = fromIntegral (ceiling (head (rands gstate) * x))