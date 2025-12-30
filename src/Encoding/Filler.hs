module Encoding.Filler (getFillerBits) where

import Shared.Model (QrConfig (..), ErrorCorrectionLevel (..))
import Utils.BitUtils (padWithZeroesRight, asBits)
import Encoding.Model (Bit (Zero))
import Data.Function ((&))

maxTerminatorSize = 4
padByte236 = asBits "11101100"
padByte17 = asBits "00010001"

getFillerBits :: QrConfig -> [Bit] -> [Bit]
getFillerBits config bits =
    bits 
        & addTerminator totalBits
        & padUntilMultipleOfEight
        & addPadBytes totalBits
    where
        totalBits = 8 * getTotalCodeWords config

addTerminator :: Int -> [Bit] -> [Bit]
addTerminator requiredSize bits = bits ++ terminator where
    terminator = take (requiredSize - currentSize) $ replicate maxTerminatorSize Zero
    currentSize = length bits

padUntilMultipleOfEight :: [Bit] -> [Bit]
padUntilMultipleOfEight bits = padWithZeroesRight nextMultiple bits where
    nextMultiple = getNextMultiple $ length bits

addPadBytes :: Int -> [Bit] -> [Bit]
addPadBytes requiredSize bits = bits ++ padBytes where
    padBytes = take (numBytes * 8) $ cycle $ padByte236 ++ padByte17
    numBytes = (requiredSize - length bits) `div` 8

getNextMultiple :: Int -> Int
getNextMultiple num
    | num `mod` 8 == 0 = num
    | otherwise    = getNextMultiple $ num + 1

-- TODO: add the rest of the cases and treat supposedly "unreachable" case

getTotalCodeWords :: QrConfig -> Int
getTotalCodeWords config =  case config of
    --        Mode  Version  ECLevel       Size
    QrConfig  _     1        Low       ->  19
    QrConfig  _     1        Medium    ->  16
    QrConfig  _     1        Quartile  ->  13
    QrConfig  _     1        High      ->  9

    QrConfig {} -> 14 -- Unreachable (version validated before this function)