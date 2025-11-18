module Matrix.Matrix (Matrix, Pixel(..), placePixels) where

import Data.Map (Map)
import qualified Data.Map as Map

data Pixel = Black | White deriving (Show)
type Coords = (Int, Int)
type Matrix = Map Coords Pixel

initialState :: (Matrix, Coords, Transition)
initialState = (Map.empty, (5, 5), placeNext)

placePixels :: [Pixel] -> Matrix
placePixels pixelStream = first $ foldl placePixel initialState pixelStream

placePixel :: (Matrix, Coords, Transition) -> Pixel -> (Matrix, Coords, Transition)
placePixel (map', coords, transition) pixel = (updatedMap, nextCoords, nextTransition) where
    updatedMap = Map.insert coords pixel map'
    (nextCoords, nextTransition) = runTransition transition coords

newtype Transition = Transition { runTransition :: Coords -> (Coords, Transition) }

placeHere :: Transition
placeHere = Transition (\(row, col) -> ((row, col), placeNext))

placeNext :: Transition
placeNext = Transition (\(row, col) -> ((row, col - 1), placeDiagonally))

placeDiagonally :: Transition
placeDiagonally = Transition (\(row, col) -> ((row - 1, col + 1), placeNext))


first :: (a, b, c) -> a
first (a, _, _) = a