{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Matrix.Placement (placePixels) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Matrix.Model (Coords, Matrix(..), Pixel)

newtype Transition = Transition { runTransition :: Coords -> Int -> (Coords, Transition) }

data PlacementState = PlacementState {
    matrix :: Matrix,
    coords :: Coords,
    transition :: Transition
}

getInitialState :: Int -> PlacementState
getInitialState size = PlacementState {
    matrix = Matrix { pixels = Map.empty, size },
    coords = (size, size),
    transition = placeLeft placeZigZaggingUpwards
}

placePixels :: [Pixel] -> Int -> Matrix
placePixels pixelStream size = matrix $ foldl placePixel initialState pixelStream where
    initialState = getInitialState size

placePixel :: PlacementState -> Pixel -> PlacementState
placePixel(PlacementState { matrix = matrix, coords = coords, transition = transition }) pixel = PlacementState { matrix = updatedMatrix, coords = nextCoords, transition = nextTransition } where
    updatedMatrix = matrix { pixels = Map.insert coords pixel matrix.pixels }
    (nextCoords, nextTransition) = runTransition transition coords matrix.size

--- Transition chains

placeLeftTwice :: Transition -> Transition
placeLeftTwice nextTransition = placeLeft $ placeLeft nextTransition

placeZigZaggingUpwards :: Transition
placeZigZaggingUpwards = placeUpAndRight $ placeLeft topBoundaryChecker

placeZigZaggingDownwards :: Transition
placeZigZaggingDownwards = placeDownAndRight $ placeLeft bottomBoundaryChecker

--- Transition "proxies"

topBoundaryChecker :: Transition
topBoundaryChecker = Transition (\(row, col) limit ->
    if row <= 1
    then runTransition (placeLeftTwice placeZigZaggingDownwards) (row, col) limit
    else runTransition placeZigZaggingUpwards (row, col) limit
  )

bottomBoundaryChecker :: Transition
bottomBoundaryChecker = Transition (\(row, col) limit ->
    if row >= limit
    then runTransition (placeLeftTwice placeZigZaggingUpwards) (row, col) limit
    else runTransition placeZigZaggingDownwards (row, col) limit
  )

--- Single transitions (curried)

placeUpAndRight :: Transition -> Transition
placeUpAndRight = placeWithDelta (-1) 1

placeDownAndRight :: Transition  -> Transition
placeDownAndRight = placeWithDelta 1 1


placeLeft :: Transition -> Transition
placeLeft = placeWithDelta 0 (-1)

--- Base transition

placeWithDelta :: Int -> Int -> Transition -> Transition
placeWithDelta deltaX deltaY nextTransition =
    Transition (\(row, col) _ -> ((row + deltaX, col + deltaY), nextTransition))