{-# LANGUAGE NamedFieldPuns #-}

module Matrix.Model (Matrix (..), Coords, Pixel (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Pixel = Black | White

type Coords = (Int, Int)

data Matrix = Matrix {pixels :: Map Coords Pixel, size :: Int}

instance Show Pixel where
  show White = "O"
  show Black = "@"

instance Show Matrix where
  show Matrix {pixels, size} = unlines $ map createRow square
    where
      createRow = concatMap getPixelFromMap
      getPixelFromMap coords = (++) " " $ maybe "." show $ Map.lookup coords pixels
      square = [[(row, col) | col <- [1 .. size]] | row <- [1 .. size]]
