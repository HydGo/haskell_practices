add a b = a + b

--Problem 1
myLast ::[a] -> a
myLast [] = error "this is empty"
myLast [x] = x
myLast (_:xs) = myLast xs


--Problem 2   -to_do
myButLast ::[a] -> a
myButLast [] = error "this is empty"
myButLast [x] = error "this is only one element"
myButLast [x,y] = y
--myButLast (_:x:xs) = myButLast (x:xs)


--Problem 3
elementAt ::[a] -> Int -> a
elementAt list n 
    | null list = error "no enough element"
    | n == 1 = head list
    | otherwise = elementAt (drop 1 list)  (n -1)

--Problem 4
myLength ::[a]-> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--Problem 5
myReverse  ::[a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


--Problem 6
isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs =  (head xs)== (last xs) &&  isPalindrome (tail (init xs)) 
--isPalindrome xs =  (head xs) == (last xs) &&  isPalindrome $ tail $ init xs


--Problem 7 
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--Problem 8
compress :: (Eq a) =>[a] -> [a]
compress [] = []
compress [x] = [x]
compress (s:sx) = if s == head sx then compress(sx) else  [s] ++ compress(sx)

--Problem 9  
pack ::(Eq a) => [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs
    in (x:first) : pack rest
pack [] = []


--Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

--Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
 
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x


--Problem 12

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x


--Problem 13   to_do
--Problem 14
dupli ::[a]->[a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


--Problem 15
repli ::[x]-> Int -> [x]
repli [] _= []
repli (x:xs) n = replicate n x ++ dupli xs


--Problem 16
dropEvery :: [a]->Int ->[a]
dropEvery s n 
        |  length s > n = (take (n-1) s) ++ dropEvery (drop n s) n
        | otherwise = s

--Problem 17 
split :: [a] -> Int -> ([a], [a])
split []         _             = ([], [])
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)
    
--Problem 18
slice :: [a]->Int-> Int ->[a]
slice (x:xs) n m
        | n > m =[]
        |m <= 0 =[]
        | n> 1 = slice xs (n-1) (m-1)
        |otherwise = x:  (slice xs  (n-1) (m-1))


--Problem 19
rotate ::[a]-> Int ->[a]
rotate l@(x:xs) n
        | n< 0 = rotate l (n + length l)
        |n == 0 = []
        |n > 0 = (slice l  (n+1)  (length l -1) ) ++ (slice l 1 n)


--Problem 20  --to_do
removeAt :: Int -> [a]->[a]
removeAt n l@(x:xs)
        | n< 1 = l
        | n> length l = l
        | n ==1 = xs
        | otherwise = x:(removeAt (n-1) xs)


--Problem 21
insertAt :: a->[a]->Int->[a]
insertAt s l@(x:xs) n 
        | n == 1 = s : l
        | otherwise = x:(insertAt s xs (n-1))


--Problem 22
range :: Int->Int ->[Int]
range n m = [n .. m]

--Problem 23  to_do
--Problem 24 to_do
--Problem 25 to_do
--Problem 26 to_do
--Problem 27 to_do
--Problem 28 to_do


--Problem 31  to_do  prime
--isPrime :: Int -> Bool
--isPrime n = 

--Problem 32
myGCD :: Int->Int->Int
myGCD n m 
    | n ==0 = m
    | otherwise = myGCD (m `mod` n)  n

--Problem 33
coprime :: Int -> Int -> Bool
coprime n m  = myGCD n m  == 1 

--Problem 34  to_do prime


--Problem 35  to_do
--primeFactors :: Int -> [Int]
--primeFactors n 

--Problem 36 to_do
--Problem 37 to_do
--Problem 38 to_do
--Problem 39  to_do
--Problem 40 to_do
--Problem 41 to_do


--Problem 46
--Problem 47 
--Problem 48 
--Problem 49
--Problem 50


--Problem 7 
--Problem 7 
--Problem 7 
--Problem 7 
