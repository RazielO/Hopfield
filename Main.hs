module Main where

import Data.List

reemplazar pos newVal list = take pos list ++ newVal : drop (pos+1) list

generarMatriz size = replicate size $ replicate size 0

cambiarMatriz m i j val = map (\x -> if x == i then reemplazar j val (m !! x) else m !! x) [0..(length m - 1)]

smult m val = [[b * val | b <- a] | a <- m]

msuma a b =  [[w + z | (w, z) <- zip x y] | (x,y) <- zip a b]

sumatoria [] result = result
sumatoria (x:xs) result = sumatoria xs (msuma x result)

multiplicar m x i j
    | i == length m = m
    | j >= length m = multiplicar m x (i + 1) 0
    | i == j = multiplicar (cambiarMatriz m i j 0) x i (j + 1)
    | otherwise = multiplicar (cambiarMatriz m i j ((x !! i) * (x !! j))) x i (j + 1)

mmult a b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

signo x
    | x < -1 && x > 1 = x
    | x >= 1 = 1
    | otherwise = -1

recuperacion w x =  do
    let x1 = map (\y -> signo (y !! 0)) (mmult w (transpose [x]))
    if x == x1
        then x
        else recuperacion w x1


main = do
    let a = [1,1,1]
    let b = [-1,-1,-1]
    let aux = [-1,-1,-1]

    let ar = multiplicar (generarMatriz (length a)) a 0 0
    let br = multiplicar (generarMatriz (length b)) b 0 0
    let w = sumatoria [ar, br] (generarMatriz (length a))

    print $ recuperacion w aux
