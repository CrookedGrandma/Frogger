-- | This module contains the data types
--   which represent the state of the game
module Model where

import Parser
import Types
import System.Random

data GameState = GameState {
                   elapsedTime  :: Float
                 , frog_pos     :: (Float, Float)
                 , frog_rot     :: Float
                 , frog_png     :: String
                 , level        :: [Lane]
                 , status       :: LevelStatus
                 , camera       :: Float
                 , cars         :: [[Car]]
                 , started      :: Bool
                 , loseTimer    :: Float
                 , loseImage    :: Int
                 , score        :: Int
                 , loseScore    :: Int
                 , highestY     :: Float
                 , savedScore   :: Bool
                 , highScreen   :: Bool
                 , shrew_pos    :: (Float, Float)
                 , shrew_rot    :: Float
                 , shrewTimer   :: Float
                 , shrewSpawned :: Bool
                 }

--The level/game is made
initialState :: GameState
initialState = GameState 0 (0, (-195)) 0 "frog.png" (parseLevel "nrRrRnlLlLnRLlnlnlnlnlnf") InProgress 0 [[]] False 0 1 0 0 0 False False (1000, 1000) 0 0 False

almostInitialState :: GameState -> GameState
almostInitialState gstate = initialState { elapsedTime = elapsedTime gstate }

--Produces a list of random Floats based on the elapsed time
rands :: GameState -> [Float]
rands gstate = randoms (mkStdGen (ceiling (elapsedTime gstate)))

--Random number from 1 to x, at rands index y
randomIntTo :: Int -> GameState -> Int -> Int
randomIntTo x gstate y = ceiling ((rands gstate !! y) * fromIntegral x)
--Random number from 1 to x, at rands index y
randomFloatTo :: Float -> GameState -> Float -> Float
randomFloatTo x gstate y = fromIntegral (ceiling ((rands gstate !! ceiling y) * x))

--Convert string to int
convert :: String -> Int
convert s = let i = read s :: Int in i