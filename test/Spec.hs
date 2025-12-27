import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Map as Map
import Matrix.Model (Matrix(..), Pixel(..))
import Matrix.Patterns (addPatterns)
import Matrix.Placement (placePixels)

main :: IO ()
main = defaultMain createTests

createTests :: TestTree
createTests = testGroup "Matrix Patterns and Placement Tests"
  [  goldenVsString
     "Creates patterns and places data bits"
     "test/matrix/patterns_and_placement.txt"
     generateMatrix
  ]

generateMatrix :: IO LBS.ByteString
generateMatrix = do
    let pixels = [ Black, White, White, Black, Black, White, White, White
                 , White, Black, White, Black, Black, Black, Black, Black
                 , White, Black, Black, Black, Black, Black, White, Black
                 , Black, Black, Black, White, Black, White, Black, White
                 , White, White, White ]
    let matrix = Matrix { pixels = Map.empty, size = 21 }
    let resultMatrix = show $ placePixels pixels $ addPatterns matrix
    return (LBS.pack resultMatrix)