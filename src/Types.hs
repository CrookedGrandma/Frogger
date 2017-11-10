module Types where

data Lane = NoCars Float
          | LeftFast Float
          | LeftSlow Float
          | RightFast Float
          | RightSlow Float
          | Finish Float

data LevelStatus = InProgress | Won
  deriving (Eq)