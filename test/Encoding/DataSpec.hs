{-# LANGUAGE OverloadedStrings #-}

module Encoding.DataSpec where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Encoding.Encoding (EncodingResult (..), encodeIntoParts)
import Shared.Error (renderError)
import Shared.Model (ErrorCorrectionLevel (Quartile), Mode (Alphanumeric), QrConfig (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils.BitUtils (asSpacedString)
import Utils.TableFormatter (renderPipeTable)

test_dataEncoding :: TestTree
test_dataEncoding =
  testGroup
    "Data encoding tests"
    [ goldenVsString
        "Encodes a message"
        "test/Encoding/encoding.md"
        (encode' "HELLO WORLD"),
      goldenVsString
        "Fails to encode lowercase"
        "test/Encoding/lowercase_error.txt"
        (encode' "hello world")
    ]

encode' :: String -> IO LBS.ByteString
encode' message =
  let qrConfig = QrConfig {mode = Alphanumeric, version = 1, ecLevel = Quartile}
      result = runExcept $ encodeIntoParts qrConfig message
   in return $ LBS.pack $ either renderError asMarkdownTable result

asMarkdownTable :: EncodingResult -> String
asMarkdownTable result = renderPipeTable ["Part", "Bitstream"] tableRows
  where
    tableRows = zipWith (\l v -> [l, v]) labels (asBitLists result)
    labels =
      [ "Mode Indicator",
        "Character Count Indicator",
        "Encoded Data",
        "Terminator",
        "Padding Bits",
        "Padding Bytes"
      ]

asBitLists :: EncodingResult -> [String]
asBitLists result = map (\field -> asSpacedString (field result)) bitParts
  where
    bitParts =
      [ modeIndicator,
        charCountIndicator,
        encodedData,
        terminator,
        paddingBits,
        paddingBytes
      ]
