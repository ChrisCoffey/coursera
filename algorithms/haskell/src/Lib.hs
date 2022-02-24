module Lib
    ( 
    karatsubaMultiplication,
    gradeSchoolMult,
    longAddition,
    numDigits
    ) where

import Data.Char (digitToInt)

import Debug.Trace 

--Assumes the two provided strings are actually integers
gradeSchoolMult :: String -> String -> String
gradeSchoolMult x y 
    | x > y     = foldl1 longAddition $ go x' (reverse y') 0
    | otherwise = foldl1 longAddition $ go y' (reverse x') 0
    where 
    x' = digitToInt <$> x
    y' = digitToInt <$> y
    mult :: [Int] -> Int -> Int -> [Int]
    mult xs m ix = ( fst $ foldr f ([], 0) xs ) ++ replicate ix 0
        where f d (ls, carry) = ((carry + (d * m)) `mod` 10 :ls, (carry + (d * m)) `div` 10)
    go :: [Int] -> [Int] -> Int -> [String]
    go xs []     _ = []
    go xs (m:ms) n = (concatMap show $ mult xs m n) : go xs ms (n + 1)

longAddition :: String -> String -> String
longAddition x y = reverse $ go (reverse x) (reverse y) 0 
    where
    go [] [] _            = []  
    go (a:as) []     tens = (reverse . show $ digitToInt a + tens) ++ as
    go []     (b:bs) tens = (reverse . show $ digitToInt b + tens) ++ bs
    go (a:as) (b:bs) tens = let
        a' = digitToInt a
        b' = digitToInt b
        in (show $ (a' + b' + tens) `mod` 10) ++ go as bs ((a' + b' + tens) `div` 10)

{- Karatsuba Multiplication
 -
 - Works by treating x & y as as the products of numbers w/ 1/2 the digits:
 -
 - Given x,y with length n
 -  x = 10^(n/2)*a + b -- mathematically splitting the digits rather than treating as a sequence
 -  y = 10^(n/2)*c + d
 -  iff a,b,c,d are n/2 digit numbers
 -
 -  This allows us to algebraicly rewrite x*y as:
 -  x*y = (10^(n/2)*a + b) * (10^(n/2)*c + d)
 -      = (10^n)*ac + 10^(n/2)*(ad + bc) + bd
 -
 -}

numDigits:: Integer -> Integer
numDigits 1000 = 4
numDigits x = floor $ logBase 10 (fromInteger x) + 1

splitDigits :: Integer -> Integer -> (Integer, Integer)
splitDigits ab n = (a,b)
    where
    a = floor $ (fromInteger ab) / 10^n
    b = ab - a * 10 ^ n

karatsubaMultiplication :: Integer -> Integer -> Integer
karatsubaMultiplication ab cd 
    | ab <= 10          = ab * cd
    | cd <= 10          = ab * cd
    | otherwise         = let
        z0 = karatsubaMultiplication b d
        z1 = karatsubaMultiplication (a + b) (c + d)
        z2 = karatsubaMultiplication a c
        --The seemingly redundant `digits 'div' 2 * 2` call is necessary b/c with small values of n it bottoms out
        in z2*10^(digits `div` 2 * 2 ) + (z1-z2-z0)*10^(digits `div` 2) + z0
    where 
    digits = max (numDigits ab) (numDigits cd)
    (a, b) = splitDigits ab (digits `div` 2)
    (c, d) = splitDigits cd (digits `div` 2)
