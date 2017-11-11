module Types where

data Lane = NoCars Float
          | LeftFast Float
          | LeftSlow Float
          | RightFast Float
          | RightSlow Float
          | Finish Float
  deriving (Eq)

data LevelStatus = InProgress | Won | Lost
  deriving (Eq)

data Direction = DUp | DDown
  deriving (Eq)

data Walker = Frog | Shrew
  deriving (Eq)

data Car = Car Float Float Float --Car X Y Speed
  deriving (Eq)