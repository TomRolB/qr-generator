module Utils.BitUtils (asBits, numAsBits, padWithZeroes, padWithZeroesRight, asString, asSpacedString) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Encoding.Model (Bit (..))
import Numeric (showBin)

asBits :: String -> [Bit]
asBits = map asBit

asBit :: Char -> Bit
asBit '0' = Zero
asBit '1' = One

numAsBits :: Int -> [Bit]
numAsBits num = asBits $ showBin num ""

padWithZeroes :: Int -> [Bit] -> [Bit]
padWithZeroes finalSize mainBits = extraBits ++ mainBits
  where
    extraBits = replicate (finalSize - length mainBits) Zero

padWithZeroesRight :: Int -> [Bit] -> [Bit]
padWithZeroesRight finalSize mainBits = mainBits ++ extraBits
  where
    extraBits = replicate (finalSize - length mainBits) Zero

asString :: [Bit] -> String
asString = concatMap asBitChar
  where
    asBitChar Zero = ['0']
    asBitChar One = ['1']

asSpacedString :: [Bit] -> String
asSpacedString bits =
  unwords $ chunksOf 8 (asString bits)
