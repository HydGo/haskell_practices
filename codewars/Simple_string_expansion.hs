import Data.Char (digitToInt)
solve :: [Char] -> [Char]
solve (x:xs) 
    |null xs = []
    |x <= '9' && x>= '1' =  (rep  (digitToInt x)  (solve xs) )
    |x == '(' || x == ')' = solve xs
    |otherwise = x: solve xs
        where rep n xs 
                   | n == 0 = []
                   |otherwise= xs ++  rep (n-1) xs