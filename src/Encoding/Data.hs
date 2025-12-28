module Encoding.Data where

import Encoding.Model (Bit, AppM)
import Utils.BitUtils
import Encoding.ConversionTables (toAlphanumericCode)
import Data.Function ((&))
import Control.Monad.Trans.Except (Except)

alphanumericBinarySize = 11
oddAlphanumericBinarySize = 6

encodeAlphanumeric :: String -> AppM [Bit]
encodeAlphanumeric message = do
    encodings <- traverse toAlphanumericCode message
    return $ multiplyPairs encodings alphanumericBinarySize -- What dictates the size?

multiplyPairs :: [Int] -> Int -> [Bit]
multiplyPairs (x1:x2:xs) binarySize = 
    (45 * x1 + x2)
        & numAsBits
        & padWithZeroes binarySize
        & (++ multiplyPairs xs binarySize)

multiplyPairs (x1:_) binarySize = padWithZeroes oddAlphanumericBinarySize $ numAsBits x1