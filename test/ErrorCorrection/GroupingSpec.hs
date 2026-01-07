module ErrorCorrection.GroupingSpec where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)
import Encoding.Model (Bit)
import ErrorCorrection.Codewords (CodewordGroup, splitIntoGroups)
import Shared.Model (ErrorCorrectionLevel (Quartile), Mode (Alphanumeric), QrConfig (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils.BitUtils (asBits, asString)
import Utils.TableFormatter (renderPipeTable)

test_dataEncoding :: TestTree
test_dataEncoding =
  testGroup
    "Data codewords grouping tests"
    [ goldenVsString
        "Groups 5-Q data bits"
        "test/ErrorCorrection/groups.md"
        getGroupsTest
    ]

getGroupsTest :: IO LBS.ByteString
getGroupsTest = do
  let qrConfig = QrConfig {mode = Alphanumeric, version = 5, ecLevel = Quartile}
  let bits = asBits "0100001101010101010001101000011001010111001001100101010111000010011101110011001000000110000100100000011001100111001001101111011011110110010000100000011101110110100001101111001000000111001001100101011000010110110001101100011110010010000001101011011011100110111101110111011100110010000001110111011010000110010101110010011001010010000001101000011010010111001100100000011101000110111101110111011001010110110000100000011010010111001100101110000011101100000100011110110000010001111011000001000111101100"
  let result = splitIntoGroups qrConfig bits
  let firstGroup = groupAsString $ fst result
  let secondGroup = groupAsString $ snd result
  return $ LBS.pack $ unlines ["# First group", "", firstGroup, "", "# Second group", "", secondGroup]

groupAsString :: CodewordGroup -> String
groupAsString group = renderPipeTable ["Block Number", "Data Codewords in the group"] rows
  where
    rows = zipWith (curry createRow) group [1 ..]

    createRow :: ([[Bit]], Int) -> [String]
    createRow (block, num) = ["Block " ++ show num, createCodewordsCell block]
    createCodewordsCell block = intercalate "<br>" $ map asString block
