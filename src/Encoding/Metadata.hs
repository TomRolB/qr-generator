{-# LANGUAGE OverloadedRecordDot #-}

module Encoding.Metadata where

import Shared.Model (Mode(..), QrConfig(..))
import Encoding.Model (Bit(..))
import Numeric (showBin)
import Utils.BitUtils

encodeData :: String -> QrConfig -> [Bit]
encodeData message config = modeIndicator ++ characterCountIndicator where
    modeIndicator = getModeIndicator config.mode
    characterCountIndicator = getCountWithPadding message config

getModeIndicator :: Mode -> [Bit]
getModeIndicator Numeric      = asBits "0001"
getModeIndicator Alphanumeric = asBits "0010"
getModeIndicator Byte         = asBits "0100"
getModeIndicator Kanji        = asBits "1000"

getCountWithPadding :: String -> QrConfig -> [Bit]
getCountWithPadding message qrConfig = padWithZeroes size mainBits where
    mainBits = asBits $ (`showBin` "") $ length message
    size = getCharacterCountIndicatorSize qrConfig

getCharacterCountIndicatorSize :: QrConfig -> Int
getCharacterCountIndicatorSize config = case config of
    --       Mode          Version         Size
    QrConfig Numeric       v | v <= 9   -> 10
    QrConfig Numeric       v | v <= 26  -> 12
    QrConfig Numeric       v | v <= 40  -> 14
    QrConfig Alphanumeric  v | v <= 9   -> 9
    QrConfig Alphanumeric  v | v <= 26  -> 11
    QrConfig Alphanumeric  v | v <= 40  -> 13
    QrConfig Byte          v | v <= 9   -> 8
    QrConfig Byte          v | v <= 26  -> 16
    QrConfig Byte          v | v <= 40  -> 16
    QrConfig Kanji         v | v <= 9   -> 8
    QrConfig Kanji         v | v <= 26  -> 10
    QrConfig Kanji         v | v <= 40  -> 12
    QrConfig _ _ -> 14 -- Unreachable (version validated before this function)
