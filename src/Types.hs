module Types where

data Lane = NoCars Float
          | LeftFast Float
          | LeftSlow Float
          | RightFast Float
          | RightSlow Float