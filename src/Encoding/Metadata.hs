{-# LANGUAGE OverloadedRecordDot #-}

module Encoding.Metadata (encodeMetadata, getModeIndicator, getCountWithPadding) where

import Encoding.Model (Bit (..))
import Numeric (showBin)
import Shared.Model (ErrorCorrectionLevel (..), Mode (..), QrConfig (..))
import Utils.BitUtils

-- TODO: handle corner cases instead of considering them unreachable

encodeMetadata :: String -> QrConfig -> [Bit]
encodeMetadata message config = modeIndicator ++ characterCountIndicator
  where
    modeIndicator = getModeIndicator config.mode
    characterCountIndicator = getCountWithPadding message config

{- ORMOLU_DISABLE -}
getModeIndicator :: Mode -> [Bit]
getModeIndicator Numeric      = asBits "0001"
getModeIndicator Alphanumeric = asBits "0010"
getModeIndicator Byte         = asBits "0100"
getModeIndicator Kanji        = asBits "1000"
{- ORMOLU_ENABLE -}

getCountWithPadding :: String -> QrConfig -> [Bit]
getCountWithPadding message qrConfig = padWithZeroes size mainBits
  where
    mainBits = asBits $ (`showBin` "") $ length message
    size = getCharacterCountIndicatorSize qrConfig

{- ORMOLU_DISABLE -}
getCharacterCountIndicatorSize :: QrConfig -> Int
getCharacterCountIndicatorSize config = case config of
    --       Mode          Version  ECLevel               Size
    QrConfig Numeric       v        _       | v <= 9   -> 10
    QrConfig Numeric       v        _       | v <= 26  -> 12
    QrConfig Numeric       v        _       | v <= 40  -> 14
    QrConfig Alphanumeric  v        _       | v <= 9   -> 9
    QrConfig Alphanumeric  v        _       | v <= 26  -> 11
    QrConfig Alphanumeric  v        _       | v <= 40  -> 13
    QrConfig Byte          v        _       | v <= 9   -> 8
    QrConfig Byte          v        _       | v <= 26  -> 16
    QrConfig Byte          v        _       | v <= 40  -> 16
    QrConfig Kanji         v        _       | v <= 9   -> 8
    QrConfig Kanji         v        _       | v <= 26  -> 10
    QrConfig Kanji         v        _       | v <= 40  -> 12
    QrConfig {} -> 14 -- Unreachable (version validated before this function)
{- ORMOLU_ENABLE -}
