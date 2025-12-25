module Main where

import qualified Data.Map as Map
import Matrix.Model (Matrix(..), Pixel(..))
import Matrix.Patterns (addFinderPatterns)

main :: IO ()
main = do
    -- print (Matrix.placePixels [Black, White, White, Black, Black, White, White, White, White, Black, White, Black, Black, Black, Black, Black] 5)
    let matrix = Matrix { pixels = Map.empty, size = 21 }
    print $ addFinderPatterns matrix
