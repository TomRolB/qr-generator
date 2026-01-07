{-# LANGUAGE NamedFieldPuns #-}

module ErrorCorrection.Codewords where

import Data.Function ((&))
import Data.List.Split (chunksOf)
import Encoding.Model (Bit)
import Shared.Model (ErrorCorrectionLevel (..), QrConfig (..))

data CodewordsGrouping = CodewordsGrouping
  { numFirstGroupBlocks :: Int,
    numFirstGroupDataCodewords :: Int,
    numSecondGroupBlocks :: Int,
    numSecondGroupDataCodewords :: Int
  }

type CodewordsGroupingTuple = (Int, Int, Int, Int)

type CodewordGroup = [[[Bit]]]

-- | Breaks data codewords into blocks and groups.
-- Grouping level is as follows, from inside out:
-- First list level: 8-bit stream
-- Second level: Block of error correction codewords
-- Third level: groups of blocks
splitIntoGroups :: QrConfig -> [Bit] -> (CodewordGroup, CodewordGroup)
splitIntoGroups config bits = (firstGroup, secondGroup)
  where
    secondGroup = buildGroup numSecondGroupDataCodewords secondGroupBits
    secondGroupBits = drop firstGroupBitsSize bits
    firstGroup = buildGroup numFirstGroupDataCodewords firstGroupBits
    firstGroupBits = take firstGroupBitsSize bits
    firstGroupBitsSize = numFirstGroupBlocks * numFirstGroupDataCodewords * 8
    CodewordsGrouping
      { numFirstGroupBlocks,
        numFirstGroupDataCodewords,
        numSecondGroupBlocks = _,
        numSecondGroupDataCodewords
      } = getCodewordsGrouping config

buildGroup :: Int -> [Bit] -> CodewordGroup
buildGroup codewords bits =
  bits
    & chunksOf 8
    & chunksOf codewords

getCodewordsGrouping :: QrConfig -> CodewordsGrouping
getCodewordsGrouping config = fromTuple $ getCodewordsGrouping' config

{- ORMOLU_DISABLE -}
fromTuple :: CodewordsGroupingTuple -> CodewordsGrouping
fromTuple (a, b, c, d) =
  CodewordsGrouping
    { numFirstGroupBlocks         = a,
      numFirstGroupDataCodewords  = b,
      numSecondGroupBlocks        = c,
      numSecondGroupDataCodewords = d
    }

getCodewordsGrouping' :: QrConfig -> CodewordsGroupingTuple
getCodewordsGrouping' config = case config of
    --        Mode  Version  ECLevel       
    QrConfig  _     1        Low       ->  (1, 19, 0, 0 )
    QrConfig  _     1        Medium    ->  (1, 16, 0, 0 )
    QrConfig  _     1        Quartile  ->  (1, 13, 0, 0 )
    QrConfig  _     1        High      ->  (1, 9,  0, 0 )
    QrConfig  _     5        Quartile  ->  (2, 15, 2, 16)
    QrConfig  {}                       ->  (0,  0, 0,  0)
