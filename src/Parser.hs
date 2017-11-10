module Parser where

import Types

parseLevel :: String -> [Lane]
parseLevel s = parseString s (-200)

parseString :: String -> Float -> [Lane]
parseString [] _     = []
parseString (x:xs) a | x == 'n'  = NoCars    a : parseString xs (a + 30)
                     | x == 'l'  = LeftSlow  a : parseString xs (a + 30)
                     | x == 'L'  = LeftFast  a : parseString xs (a + 30)
                     | x == 'r'  = RightSlow a : parseString xs (a + 30)
                     | x == 'R'  = RightFast a : parseString xs (a + 30)
                     | otherwise =               parseString xs (a + 30)