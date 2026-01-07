module ErrorCorrection.GaloisSpec where

import qualified Data.ByteString.Lazy.Char8 as LBS
import ErrorCorrection.Polynomial (dividePolynomials, getAntilog, getAntilogs, getLog)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_dataEncoding :: TestTree
test_dataEncoding =
  testGroup
    "Galois mathematics tests"
    [ goldenVsString
        "Checks antilogs are correct"
        "test/ErrorCorrection/antilogs.md"
        getAntilogsTest,
      goldenVsString
        "Checks logs are correct"
        "test/ErrorCorrection/logs.md"
        getLogsTest,
      goldenVsString
        "Divides polynomials (1 round)"
        "test/ErrorCorrection/polynomialDivisionFirstRound.md"
        (dividePolynomialsTest 1),
      goldenVsString
        "Divides polynomials (full)"
        "test/ErrorCorrection/polynomialDivision.md"
        (dividePolynomialsTest 16)
    ]

getAntilogsTest :: IO LBS.ByteString
getAntilogsTest = do
  return $ LBS.pack $ unwords $ map (show . getAntilog) [0 .. 258]

getLogsTest :: IO LBS.ByteString
getLogsTest = do
  return $ LBS.pack $ unwords $ map (show . getLog . getAntilog) [0 .. 258]

dividePolynomialsTest :: Int -> IO LBS.ByteString
dividePolynomialsTest rounds = do
  let messagePolynomial = [32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236, 17, 236, 17]
  let generatorPolynomial = getAntilogs [1, 251, 67, 46, 61, 118, 70, 64, 94, 32, 45]
  return $ LBS.pack $ unwords $ map show $ dividePolynomials messagePolynomial generatorPolynomial rounds
