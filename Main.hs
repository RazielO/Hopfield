module Main where

import Data.List

replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

generateMatrix size = replicate size $ replicate size 0

updateMatrix m i j val = map (\x -> if x == i then replace j val (m !! x) else m !! x) [0..(length m - 1)]

smult m val = [[b * val | b <- a] | a <- m]

msum a b =  [[w + z | (w, z) <- zip x y] | (x,y) <- zip a b]

summation [] result = result
summation (x:xs) result = summation xs (msum x result)

multiply m x i j
    | i == length m = m
    | j >= length m = multiply m x (i + 1) 0
    | i == j = multiply (updateMatrix m i j 0) x i (j + 1)
    | otherwise = multiply (updateMatrix m i j ((x !! i) * (x !! j))) x i (j + 1)

mmult a b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

sign x
    | x < -1 && x > 1 = x
    | x >= 1 = 1
    | otherwise = -1

recuperacion w x =  do
    let x1 = map (\y -> sign (y !! 0)) (mmult w (transpose [x]))
    if x == x1
        then x
        else recuperacion w x1


main = do
    let a = [1,1,1]
    let b = [-1,-1,-1]
    let aux = [-1,-1,-1]

    let ar = multiply (generateMatrix (length a)) a 0 0
    let br = multiply (generateMatrix (length b)) b 0 0
    let w = summation [ar, br] (generateMatrix (length a))

    print $ recuperacion w aux
