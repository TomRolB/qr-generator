module Encoding.Data where

import Encoding.Model (Bit, AppM)
import Utils.BitUtils
import Encoding.ConversionTables (toAlphanumericCode)
import Data.Function ((&))
import Control.Monad.Trans.Except (Except)

encodeAlphanumeric :: String -> AppM [Bit]
encodeAlphanumeric message = do
    encodings <- traverse toAlphanumericCode message
    return $ multiplyPairs encodings 11 -- What dictates the size?

multiplyPairs :: [Int] -> Int -> [Bit]
multiplyPairs (x1:x2:xs) binarySize = 
    (45 * x1 + x2)
        & numAsBits
        & padWithZeroes binarySize
        & (++ multiplyPairs xs binarySize)

multiplyPairs (x1:_) binarySize = padWithZeroes 6 $ numAsBits x1