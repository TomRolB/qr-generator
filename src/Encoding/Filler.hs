module Encoding.Filler (getTerminator, padUntilMultipleOfEight, getPaddingBytes, getTotalCodeWords) where

import Encoding.Model (Bit (Zero))
import Shared.Model (ErrorCorrectionLevel (..), QrConfig (..))
import Utils.BitUtils (asBits)

maxTerminatorSize :: Int
maxTerminatorSize = 4

padByte236 :: [Bit]
padByte236 = asBits "11101100"

padByte17 :: [Bit]
padByte17 = asBits "00010001"

getTerminator :: Int -> Int -> [Bit]
getTerminator requiredSize currentSize =
  take (requiredSize - currentSize) $ replicate maxTerminatorSize Zero

padUntilMultipleOfEight :: Int -> [Bit]
padUntilMultipleOfEight currentSize = replicate (nextMultiple - currentSize) Zero
  where
    nextMultiple = getNextMultiple currentSize

getPaddingBytes :: Int -> Int -> [Bit]
getPaddingBytes requiredSize currentSize = padBytes
  where
    padBytes = take numBits $ cycle $ padByte236 ++ padByte17
    numBits = requiredSize - currentSize

getNextMultiple :: Int -> Int
getNextMultiple num
  | num `mod` 8 == 0 = num
  | otherwise = getNextMultiple $ num + 1

-- TODO: add the rest of the cases and treat supposedly "unreachable" case

{- ORMOLU_DISABLE -}
getTotalCodeWords :: QrConfig -> Int
getTotalCodeWords config =  case config of
    --        Mode  Version  ECLevel       Size
    QrConfig  _     1        Low       ->  19
    QrConfig  _     1        Medium    ->  16
    QrConfig  _     1        Quartile  ->  13
    QrConfig  _     1        High      ->  9

    QrConfig {} -> 14 -- Unreachable (version validated before this function)
