module Types where

--All the different types of lanes and their Y coordinate
data Lane = NoCars Float
          | LeftFast Float
          | LeftSlow Float
          | RightFast Float
          | RightSlow Float
          | Finish Float
  deriving (Eq)

--All possible statuses
data LevelStatus = Intro | InProgress | Won | Losing | Lost | Paused
  deriving (Eq)

--Speaks for itself
data Direction = DUp | DDown | DLeft | DRight
  deriving (Eq)

--Either a Frog or a Shrew, used in the movement functions
data Walker = Frog | Shrew
  deriving (Eq)

--Car coming from either the right side or the left side with their X and Y coordinates, followed by their speed
data Car = CarL Float Float Float
         | CarR Float Float Float
         | Error
  deriving (Eq)