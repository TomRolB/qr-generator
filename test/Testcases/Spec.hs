{-# LANGUAGE OverloadedStrings #-}

module Testcases.Spec where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import ErrorCorrection.Polynomial (getGeneratorPolynomial)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

marker :: T.Text
marker = "<!-- EXPECTED -->"

-- NOTE: tasty-discover expects TestTree values named `test_*`.
-- We keep discovery at top-level via unsafePerformIO so tests are fully dynamic.

test_testcases :: TestTree
test_testcases = _setUtf8 `seq` testGroup "Markdown testcases" (map mkTestcase discovered)

{-# NOINLINE _setUtf8 #-}
_setUtf8 :: ()
_setUtf8 = unsafePerformIO $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pure ()

{-# NOINLINE discovered #-}
discovered :: [(FilePath, TestName)]
discovered = unsafePerformIO discoverTestcases

mkTestcase :: (FilePath, TestName) -> TestTree
mkTestcase (path, name) = goldenVsString name path (runTestcase path)

runTestcase :: FilePath -> IO LBS.ByteString
runTestcase path = do
  contents <- BS.readFile path
  let newline = detectNewlineBytes contents
  case splitOnMarkerBytes contents of
    Nothing -> fail $ "Missing marker '" <> T.unpack marker <> "' in " <> path
    Just (beforeMarkerBytes, _afterMarkerBytes) -> do
      let beforeMarkerText = TE.decodeUtf8With TEE.lenientDecode beforeMarkerBytes
      let header = parseHeader beforeMarkerText
      output <- runStrategy (tcType header) beforeMarkerText
      let finalBytes = beforeMarkerBytes <> markerBytes <> newline <> encodeWithNewline newline output
      pure $ LBS.fromStrict finalBytes

-- === Testcase discovery ===

discoverTestcases :: IO [(FilePath, TestName)]
discoverTestcases = do
  files <- listRecursive "test"
  let testcaseFiles = List.sort [p | p <- files, ".testcase.md" `List.isSuffixOf` p]
  mapM testcaseNameFromFile testcaseFiles

testcaseNameFromFile :: FilePath -> IO (FilePath, TestName)
testcaseNameFromFile path = do
  contents <- BS.readFile path
  beforeMarkerBytes <-
    case splitOnMarkerBytes contents of
      Nothing -> fail $ "Missing marker '" <> T.unpack marker <> "' in " <> path
      Just (before, _) -> pure before
  let beforeMarkerText = TE.decodeUtf8With TEE.lenientDecode beforeMarkerBytes
  let header = parseHeader beforeMarkerText
  pure (path, sanitizeTestName (tcName header))

sanitizeTestName :: T.Text -> String
sanitizeTestName = map sanitizeChar . T.unpack
  where
    sanitizeChar '—' = '-'
    sanitizeChar c
      | Char.ord c < 128 = c
      | otherwise = '?'

listRecursive :: FilePath -> IO [FilePath]
listRecursive root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      paths <-
        mapM
          ( \e -> do
              let p = dir </> e
              isDir <- doesDirectoryExist p
              if isDir then go p else pure [p]
          )
          entries
      pure (concat paths)

-- === Parsing / framework ===

data TestcaseHeader = TestcaseHeader
  { tcType :: !T.Text,
    tcName :: !T.Text
  }

afterStripCR :: T.Text -> T.Text
afterStripCR = T.dropWhileEnd (== '\r')

parseHeader :: T.Text -> TestcaseHeader
parseHeader preMarker =
  case map afterStripCR (T.lines preMarker) of
    (l1 : l2 : _) ->
      let ty = parseTaggedLine "@type" l1
          nm = parseTaggedLine "@name" l2
       in TestcaseHeader {tcType = ty, tcName = nm}
    _ -> error "A .testcase.md file must have at least 2 lines: @type and @name"

parseTaggedLine :: T.Text -> T.Text -> T.Text
parseTaggedLine tag line
  | (tag <> ":") `T.isPrefixOf` line = T.strip (T.drop (T.length tag + 1) line)
  | (tag <> " ") `T.isPrefixOf` line = T.strip (T.drop (T.length tag + 1) line)
  | otherwise = error $ "Expected '" <> T.unpack tag <> "' on line: " <> T.unpack line

markerBytes :: BS.ByteString
markerBytes = TE.encodeUtf8 marker

splitOnMarkerBytes :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitOnMarkerBytes contents =
  let (before, rest) = BS.breakSubstring markerBytes contents
   in if BS.null rest
        then Nothing
        else Just (before, BS.drop (BS.length markerBytes) rest)

detectNewlineBytes :: BS.ByteString -> BS.ByteString
detectNewlineBytes bs
  | BS.pack [13, 10] `BS.isInfixOf` bs = BS.pack [13, 10]
  | otherwise = BS.pack [10]

encodeWithNewline :: BS.ByteString -> T.Text -> BS.ByteString
encodeWithNewline newline text
  | newline == BS.pack [13, 10] = BS.intercalate newline (BS.split 10 (TE.encodeUtf8 text))
  | otherwise = TE.encodeUtf8 text

runStrategy :: T.Text -> T.Text -> IO T.Text
runStrategy ty preMarker
  | ty == "generator-polynomial" = pure (strategyGeneratorPolynomial preMarker)
  | otherwise = fail $ "Unknown testcase type: " <> T.unpack ty

-- === Strategies ===

strategyGeneratorPolynomial :: T.Text -> T.Text
strategyGeneratorPolynomial preMarker =
  let n = parseIntField ["n", "degree", "ec-codewords"] preMarker
      coeffs = getGeneratorPolynomial n
   in T.unlines
        [ "```txt",
          "g(x) = " <> renderPolynomial coeffs,
          "```"
        ]

renderPolynomial :: [Int] -> T.Text
renderPolynomial coeffs =
  let degree = length coeffs - 1
      terms =
        [ renderTerm c e
          | (c, e) <- zip coeffs [degree, degree - 1 .. 0],
            c /= 0
        ]
   in if null terms then "0" else T.intercalate " + " terms

renderTerm :: Int -> Int -> T.Text
renderTerm coeff power
  | power <= 0 = T.pack (show coeff)
  | power == 1 =
      if coeff == 1
        then "x"
        else T.pack (show coeff) <> "x"
  | otherwise =
      if coeff == 1
        then "x" <> T.pack (show power)
        else T.pack (show coeff) <> "x" <> T.pack (show power)

parseIntField :: [T.Text] -> T.Text -> Int
parseIntField keys input =
  case List.find (not . T.null) (map (findKey keys . afterStripCR) (T.lines input)) of
    Nothing -> error $ "Missing int field. Tried keys: " <> show (map T.unpack keys)
    Just v ->
      case reads (T.unpack v) of
        [(n, "")] -> n
        _ -> error $ "Failed to parse int from: " <> T.unpack v
  where
    findKey :: [T.Text] -> T.Text -> T.Text
    findKey ks line =
      case List.find (\k -> (k <> ":") `T.isPrefixOf` T.strip line) ks of
        Nothing -> ""
        Just k -> T.strip $ T.drop (T.length k + 1) (T.strip line)
