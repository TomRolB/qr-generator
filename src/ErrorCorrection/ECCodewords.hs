module ErrorCorrection.ECCodewords (dividePolynomials, getAntilog, getAntilogs, getLog, getLogs) where

import Data.Bits
import qualified Data.Map as Map
import Utils.MapUtils (invertMap)

-- | Takes the coefficients of polynomials, whose grade is the length of the corresponding list
dividePolynomials :: [Int] -> [Int] -> Int -> [Int]
dividePolynomials [] _ _ = []
dividePolynomials _ [] _ = [] -- Generator polynomial can never be zero, so this is just for safe matching
dividePolynomials dividend _ 0 = dividend
dividePolynomials dividend@(x : _) divisor rounds = dividePolynomials xored divisor $ rounds - 1
  where
    xored = drop 1 $ xorWithDefaults dividend divisorPlusLead
    divisorPlusLead = map (getAntilog . (`mod` 255) . (+ leadLog)) divisorLogs
    leadLog = getLog x
    divisorLogs = getLogs divisor

xorWithDefaults :: [Int] -> [Int] -> [Int]
xorWithDefaults a b = take maxLen $ zipWith xor infiniteA infiniteB
  where
    maxLen = max (length a) (length b)
    infiniteA = a ++ repeat 0
    infiniteB = b ++ repeat 0

getAntilogs :: [Int] -> [Int]
getAntilogs = map getAntilog

getLogs :: [Int] -> [Int]
getLogs = map getLog

getAntilog :: Int -> Int
getAntilog num = Map.findWithDefault (-1) (num `mod` 255) numToAntilogMap

getLog :: Int -> Int
getLog num = Map.findWithDefault (-1) num numToLogMap

numToLogMap :: Map.Map Int Int
numToLogMap = invertMap numToAntilogMap

numToAntilogMap :: Map.Map Int Int
numToAntilogMap = Map.fromList $ zip [0 .. 254] antilogs
  where
    antilogs = take 255 $ iterate xoredMul 1

xoredMul :: Int -> Int
xoredMul lastPower
  | rawPower >= 256 = rawPower `xor` 285
  | otherwise = rawPower
  where
    rawPower = 2 * lastPower
