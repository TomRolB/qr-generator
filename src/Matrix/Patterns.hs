{-# LANGUAGE OverloadedRecordDot #-}

module Matrix.Patterns (addFinderPatterns) where

import Matrix.Model
import qualified Data.Map as Map
import Data.Function ((&))

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

addBlackCenter :: Coords -> Matrix -> Matrix
addBlackCenter (row, col) matrix = setPixels matrix center Black where
    center = liftA2 (,) [row-1..row+1] [col-1..col+1]

addWhiteRing :: Coords -> Matrix -> Matrix
addWhiteRing (row, col) matrix = setPixels matrix innerRing White where
    innerRing = generateRing (row, col) 2

addBlackRing :: Coords -> Matrix -> Matrix
addBlackRing (row, col) matrix = setPixels matrix outerRing Black where
    outerRing = generateRing (row, col) 3

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

