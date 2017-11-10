-- | This module contains the data types
--   which represent the state of the game
module Model where

import Parser
import Types

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , frog_pos    :: (Float, Float)
                 , frog_rot    :: Float
                 , level       :: [Lane]
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 (0, (-195)) 0 (parseLevel "nrRrRnlLlLnRLf")