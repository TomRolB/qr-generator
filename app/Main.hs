module Main where

import Matrix.Matrix as Matrix

main :: IO ()
main = do
    print (Matrix.placePixels [Black, White, White, Black, Black] 5)
