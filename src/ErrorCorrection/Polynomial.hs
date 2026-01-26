module ErrorCorrection.Polynomial (dividePolynomials, getGeneratorPolynomial, getAntilog, getAntilogs, getLog, getLogs) where

import Data.Bits
import qualified Data.Map as Map
import Utils.MapUtils (invertMap)

type GradedPolynomial = [(Int, Int)] -- Represents [(coefficient, grade)]

-- | Takes the coefficients of polynomials, whose grade is the length of the corresponding list
dividePolynomials :: [Int] -> [Int] -> Int -> [Int]
dividePolynomials [] _ _ = []
dividePolynomials _ [] _ = [] -- Generator polynomial will never be zero, so this is just for safe matching
dividePolynomials dividend _ 0 = dividend
dividePolynomials dividend@(x : _) divisor rounds = dividePolynomials xored divisor $ rounds - 1
  where
    xored = drop 1 $ xorWithDefaults dividend divisorPlusLead
    divisorPlusLead = map (getAntilog . (`mod` 255) . (+ leadLog)) divisorLogs
    leadLog = getLog x
    divisorLogs = getLogs divisor

getGeneratorPolynomial :: Int -> [Int]
getGeneratorPolynomial n = toDense $ foldl multiplyGraded [(1, 0)] [0 .. n - 1]
  where
    toDense :: GradedPolynomial -> [Int]
    toDense poly =
      let maxGrade = if null poly then 0 else maximum (map snd poly)
          termMap = Map.fromList (map (\(c, g) -> (g, c)) poly)
       in [Map.findWithDefault 0 g termMap | g <- [maxGrade, maxGrade - 1 .. 0]]

    multiplyGraded :: GradedPolynomial -> Int -> GradedPolynomial
    multiplyGraded poly i =
      let factor = [(1, 1), (getAntilog i, 0)] -- (x + a^i)
          terms = [(galoisMultiply c1 c2, g1 + g2) | (c1, g1) <- poly, (c2, g2) <- factor]
       in simplify terms

    simplify :: GradedPolynomial -> GradedPolynomial
    simplify terms =
      filter (\(c, _) -> c /= 0) $
        map (\(g, c) -> (c, g)) $
          Map.toList $
            Map.fromListWith xor [(g, c) | (c, g) <- terms]

    galoisMultiply a b = getAntilog $ (getLog a + getLog b) `mod` 255 -- We filter 0s so this is safe

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
