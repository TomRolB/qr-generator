{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Encoding.Encoding where

import Encoding.Data (encodeAlphanumeric)
import Encoding.Filler (getPaddingBytes, getTerminator, getTotalCodeWords, padUntilMultipleOfEight)
import Encoding.Metadata (getCountWithPadding, getModeIndicator)
import Encoding.Model (AppM, Bit)
import Shared.Model (QrConfig (..))

data EncodingResult = EncodingResult
  { modeIndicator :: [Bit],
    charCountIndicator :: [Bit],
    encodedData :: [Bit],
    terminator :: [Bit],
    paddingBits :: [Bit],
    paddingBytes :: [Bit]
  }

encode :: QrConfig -> String -> AppM [Bit]
encode qrConfig message = do
  encoded <- encodeIntoParts qrConfig message
  return $ ensembleEncoding encoded

encodeIntoParts :: QrConfig -> String -> AppM EncodingResult
encodeIntoParts qrConfig message = do
  encodedData <- encodeAlphanumeric message

  let modeIndicator = getModeIndicator qrConfig.mode
  let charCountIndicator = getCountWithPadding message qrConfig
  let combined = modeIndicator ++ charCountIndicator ++ encodedData
  let combinedLen = length combined

  let totalBitsRequired = 8 * getTotalCodeWords qrConfig
  let terminator = getTerminator totalBitsRequired combinedLen

  let withTerminatorLen = combinedLen + length terminator
  let paddingBits = padUntilMultipleOfEight withTerminatorLen

  let paddingBitsLen = withTerminatorLen + length paddingBits
  let paddingBytes = getPaddingBytes totalBitsRequired paddingBitsLen

  return
    EncodingResult
      { modeIndicator,
        charCountIndicator,
        encodedData,
        terminator,
        paddingBits,
        paddingBytes
      }

ensembleEncoding :: EncodingResult -> [Bit]
ensembleEncoding r =
  concatMap
    ($ r)
    [ modeIndicator,
      charCountIndicator,
      encodedData,
      terminator,
      paddingBits,
      paddingBytes
    ]
