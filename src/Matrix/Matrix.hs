module Matrix.Matrix (Matrix, Pixel(..), placePixels) where

import Data.Map (Map)
import qualified Data.Map as Map

data Pixel = Black | White deriving (Show)
type Coords = (Int, Int)
type Matrix = Map Coords Pixel

placePixels :: [Pixel] -> Matrix
placePixels pixelStream = foldl placeModule Map.empty (zip [0..] pixelStream) where
    placeModule map' (index, pixel) = Map.insert (index, index) pixel map' 
