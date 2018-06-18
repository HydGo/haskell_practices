module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount l
    |null l = 0
    |head l == 'a'||head l == 'e' || head l== 'i' || head l== 'o' || head l == 'u' = 1 + getCount (tail l)
    |otherwise =  getCount (tail l)


getCount' :: String -> Int
getCount' l = length $ filter (`elem` "aeiou") l