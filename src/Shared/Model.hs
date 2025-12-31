module Shared.Model (Mode (..), ErrorCorrectionLevel (..), QrConfig (..)) where

data Mode = Numeric | Alphanumeric | Byte | Kanji deriving (Show, Eq)

data ErrorCorrectionLevel = Low | Medium | Quartile | High deriving (Show, Eq)

-- TODO: validate on the very beginning that the version has valid values.

data QrConfig = QrConfig
  { mode :: Mode,
    version :: Int,
    ecLevel :: ErrorCorrectionLevel
  }
