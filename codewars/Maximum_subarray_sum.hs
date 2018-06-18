maxSequence1 :: [Int] ->Int -> Int ->Int
maxSequence l = maxSequence1 l 0 0
maxSequence1 l sum maxsum
    |null l = sum
    |sum > 0 =   maxSequence1 ( tail l) (sum + head l) (max (sum + head l) maxsum)
    |otherwise =  maxSequence1 ( tail l) (head l) (max (head l) maxsum)
    
