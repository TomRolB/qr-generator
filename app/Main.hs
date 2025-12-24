module Main where

import Matrix.Matrix as Matrix

main :: IO ()
main = do
    print (Matrix.placePixels [Black, White, White, Black, Black, White, White, White, White, Black, White, Black, Black, Black, Black, Black] 5)
