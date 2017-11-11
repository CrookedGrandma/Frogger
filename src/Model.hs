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
                 , started     :: Bool
                 }

initialState :: GameState
initialState = GameState 0 (0, (-195)) 0 (parseLevel "nrRrRnlLlLnRLlnlnlnlnlnf") InProgress 0 [[]] False

almostInitialState :: GameState -> GameState
almostInitialState gstate = initialState { elapsedTime = elapsedTime gstate }

rands :: GameState -> [Float]
rands gstate = randoms (mkStdGen (ceiling (elapsedTime gstate)))

-- | random number from 1 to x, at rands index y
randomIntTo :: Int -> GameState -> Int -> Int
randomIntTo x gstate y = ceiling ((rands gstate !! y) * fromIntegral x)
-- | random number from 1 to x, at rands index y
randomFloatTo :: Float -> GameState -> Float -> Float
randomFloatTo x gstate y = fromIntegral (ceiling ((rands gstate !! ceiling y) * x))