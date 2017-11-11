module Parser where

import Types

parseLevel :: String -> [Lane]
parseLevel s = parseLevel' s (-195)

parseLevel' :: String -> Float -> [Lane]
parseLevel' [] _     = []
parseLevel' (x:xs) a | x == 'n'  = NoCars    a : parseLevel' xs (a + 30)
                     | x == 'l'  = LeftSlow  a : parseLevel' xs (a + 30)
                     | x == 'L'  = LeftFast  a : parseLevel' xs (a + 30)
                     | x == 'r'  = RightSlow a : parseLevel' xs (a + 30)
                     | x == 'R'  = RightFast a : parseLevel' xs (a + 30)
                     | x == 'f'  = [Finish a]
                     | otherwise =               parseLevel' xs  a