module Main where

import Matrix.Matrix as Matrix
-- import Data.Set (Set, empty, insert, member)

-- mySet :: Set Int
-- mySet =
--     insert 1 $
--     insert 5 $
--     insert 3 empty

main :: IO ()
main = do
    -- putStrLn $ "Is 3 a member? " ++ show (member 3 mySet)
    -- putStrLn $ "Is 4 a member? " ++ show (member 4 mySet)
    print (Matrix.placePixels [Black, White])
