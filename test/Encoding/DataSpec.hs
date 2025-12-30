module Encoding.DataSpec where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Encoding.Metadata (encodeMetadata)
import Encoding.Data (encodeAlphanumeric)
import Shared.Model (QrConfig (..), Mode (Alphanumeric), ErrorCorrectionLevel (Quartile))
import Shared.Error (renderError)
import Utils.BitUtils (asString)

import Control.Monad.Trans.Except (runExcept)
import Encoding.Filler (getFillerBits)

test_dataEncoding :: TestTree
test_dataEncoding = testGroup "Data encoding tests"
  [ goldenVsString
      "Encodes a message"
      "test/Encoding/encoding.txt"
      (encode "HELLO WORLD")
  , goldenVsString
      "Fails to encode lowercase"
      "test/Encoding/lowercase_error.txt"
      (encode "hello world")
  ]

encode :: String -> IO LBS.ByteString
encode message = do
  let qrConfig = QrConfig { mode = Alphanumeric, version = 1, ecLevel = Quartile }

  let result = runExcept $ do
        dataBits     <- encodeAlphanumeric message
        let metadata = encodeMetadata message qrConfig
        let combined = metadata ++ dataBits
        let withFiller = getFillerBits qrConfig combined
        return $ asString withFiller

  -- Finally, handle the single result point
  case result of
    Left err  -> return $ LBS.pack $ renderError err
    Right str -> return $ LBS.pack str