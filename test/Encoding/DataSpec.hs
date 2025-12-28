{-# LANGUAGE NamedFieldPuns #-}
module Encoding.DataSpec where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Encoding.Metadata (encodeMetadata)
import Encoding.Data (encodeAlphanumeric)
import Shared.Model (QrConfig (..), Mode (Alphanumeric))
import Shared.Error (renderError)
import Utils.BitUtils (asString)

import Control.Monad.Trans.Except (runExcept)

test_dataEncoding :: TestTree
test_dataEncoding = testGroup "Data encoding tests"
  [  goldenVsString
     "Encodes a message"
     "test/Encoding/encoding.txt"
     encode
  ]

encode :: IO LBS.ByteString
encode = do
  let qrConfig = QrConfig { mode = Alphanumeric, version = 1 }
  let message = "HELLO WORLD"
  let metadataBits = encodeMetadata message qrConfig
  let eitherErrorOrData = runExcept $ encodeAlphanumeric message
  case eitherErrorOrData of
    Left err -> return $ LBS.pack $ renderError err
    Right dataBits -> return $ LBS.pack $ asString $ metadataBits ++ dataBits  