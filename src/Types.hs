module Types where

data Lane = NoCars Float
          | LeftFast Float
          | LeftSlow Float
          | RightFast Float
          | RightSlow Float
          | Finish Float
  deriving (Eq)

data LevelStatus = InProgress | Won | Losing | Lost | Paused
  deriving (Eq)

data Direction = DUp | DDown
  deriving (Eq)

data Walker = Frog | Shrew
  deriving (Eq)

data Car = CarL Float Float Float --Car X Y Speed
         | CarR Float Float Float
         | Error
  deriving (Eq)