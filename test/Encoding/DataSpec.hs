{-# LANGUAGE OverloadedStrings #-}

module Encoding.DataSpec where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Encoding.Metadata (encodeMetadata)
import Encoding.Data (encodeAlphanumeric)
import Shared.Model (QrConfig (..), Mode (Alphanumeric), ErrorCorrectionLevel (Quartile))
import Shared.Error (renderError)
import Utils.BitUtils (asSpacedString)

import Control.Monad.Trans.Except (runExcept, runExceptT)
import Encoding.Model (Bit)
import Encoding.Encoding (encodeIntoParts, EncodingResult (..))

import qualified Data.Text as T
import Text.Pandoc (runPure, writeMarkdown, def, PandocPure, WriterOptions (writerExtensions, writerColumns))
import Text.Pandoc.Builder (doc, simpleTable, plain, str)
import Text.Pandoc.Options (extensionsFromList, Extension(Ext_pipe_tables))
import Text.Printf (printf)
import qualified GHC.TypeError as T
import Data.List (transpose)

test_dataEncoding :: TestTree
test_dataEncoding = testGroup "Data encoding tests"
  [ goldenVsString
      "Encodes a message"
      "test/Encoding/encoding.md"
      (encode' "HELLO WORLD")
  , goldenVsString
      "Fails to encode lowercase"
      "test/Encoding/lowercase_error.txt"
      (encode' "hello world")
  ]

encode' :: String -> IO LBS.ByteString
encode' message =
  let qrConfig = QrConfig { mode = Alphanumeric, version = 1, ecLevel = Quartile }
      result   = runExcept $ encodeIntoParts qrConfig message
  in return $ LBS.pack $ either renderError asMarkdownTable result


asMarkdownTable :: EncodingResult -> String
asMarkdownTable result =
  case runPure (buildTable result) of
    Left _  -> "Error generating markdown"
    Right t -> T.unpack t

buildTable :: EncodingResult -> PandocPure T.Text
buildTable result = writeMarkdown customOptions $ doc table
  where
    customOptions = def
      { writerExtensions = extensionsFromList [Ext_pipe_tables]
      , writerColumns = 200
      }

    rawLabels = ["Mode Indicator", "Character Count Indicator", "Encoded Data", "Terminator", "Padding Bits", "Padding Bytes"]
    rawValues = asBitLists result

    (paddedHeaders, paddedRows) = formatTableData ["Part", "Bitstream"] (zipWith (\l v -> [l, v]) rawLabels rawValues)

    table = simpleTable (map (plain . str . T.pack) paddedHeaders)
                        (map (map (plain . str . T.pack)) paddedRows)

asBitLists :: EncodingResult -> [String]
asBitLists result = map (\field -> asSpacedString (field result)) bitParts
  where
    bitParts = [modeIndicator, charCountIndicator, encodedData, terminator, paddingBits, paddingBytes]

formatTableData :: [String] -> [[String]] -> ([String], [[String]])
formatTableData headers rows = (paddedHeaders, paddedRows)
  where
    allRows = headers : rows
    columnWidths = map (maximum . map length) (transpose allRows)

    padRow = zipWith (printf "%-*s") columnWidths

    paddedHeaders = padRow headers
    paddedRows    = map padRow rows
