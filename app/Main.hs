module Main where

import qualified Data.Map as Map
import Matrix.Model (Matrix(..), Pixel(..))
import Matrix.Patterns (addPatterns)
import Matrix.Placement (placePixels)

main :: IO ()
main = do
    let examplePixels = [ Black, White, White, Black, Black, White, White, White
                 , White, Black, White, Black, Black, Black, Black, Black
                 , White, Black, Black, Black, Black, Black, White, Black
                 , Black, Black, Black, White, Black, White, Black, White ]
    let matrix = Matrix { pixels = Map.empty, size = 21 }
    print $ placePixels examplePixels $ addPatterns matrix
