{-# LANGUAGE OverloadedRecordDot #-}

module Matrix.Patterns (addPatterns) where

import Matrix.Model
import qualified Data.Map as Map
import Data.Function ((&))

addPatterns :: Matrix -> Matrix
addPatterns matrix = 
    matrix
        & addFinderPatterns
        & addSeparators

addFinderPatterns :: Matrix -> Matrix
addFinderPatterns matrix =
    matrix
        & addFinderPattern (4, 4) 
        & addFinderPattern (4, matrix.size - 3) 
        & addFinderPattern (matrix.size - 3, 4) 

addFinderPattern :: Coords -> Matrix -> Matrix
addFinderPattern centerCoords matrix = 
    matrix
        & addBlackCenter centerCoords
        & addWhiteRing centerCoords
        & addBlackRing centerCoords

addSeparators :: Matrix -> Matrix
addSeparators matrix = 
    matrix
        & addLine (8, 8) (-1, 0) 8
        & addLine (8, 7) (0, -1) 7
        & addLine (8, matrix.size - 7) (-1, -0) 8
        & addLine (8, matrix.size - 7) (0, 1) 7
        & addLine (matrix.size - 7, 8) (1, 0) 8
        & addLine (matrix.size - 7, 8) (0, -1) 7

addBlackCenter :: Coords -> Matrix -> Matrix
addBlackCenter (row, col) matrix = setPixels matrix center Black where
    center = liftA2 (,) [row-1..row+1] [col-1..col+1]

addWhiteRing :: Coords -> Matrix -> Matrix
addWhiteRing (row, col) matrix = setPixels matrix innerRing White where
    innerRing = generateRing (row, col) 2

addBlackRing :: Coords -> Matrix -> Matrix
addBlackRing (row, col) matrix = setPixels matrix outerRing Black where
    outerRing = generateRing (row, col) 3

addLine :: Coords -> Coords -> Int -> Matrix -> Matrix
addLine fromCoords deltaCoords len matrix = setPixels matrix line White where
    line = generateLine fromCoords deltaCoords len

setPixels :: Matrix -> [Coords] -> Pixel -> Matrix
setPixels matrix coords color = matrix { pixels = newPixels } where
    newPixels = foldr (`Map.insert` color) matrix.pixels coords

generateRing :: Coords -> Int -> [Coords]
generateRing (row, col) radius = upper ++ lower ++ right ++ left where
    upper = generateLine (row - radius, col - radius) (0, 1) (2 * radius)
    lower = generateLine (row + radius, col + radius) (0, -1) (2 * radius)
    left = generateLine (row + radius, col - radius) (-1, 0) (2 * radius)
    right = generateLine (row - radius, col + radius) (1, 0) (2 * radius)

generateLine :: Coords -> Coords -> Int -> [Coords]
generateLine (fromRow, fromCol) (deltaRow, deltaCol) size =
    [(fromRow + deltaRow * x, fromCol + deltaCol * x) | x <- [0..size]]

