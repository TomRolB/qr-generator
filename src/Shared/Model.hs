module Shared.Model (Mode(..), QrConfig(..)) where

data Mode = Numeric | Alphanumeric | Byte | Kanji deriving (Show, Eq)

-- TODO: validate on the very beginning that the version has valid values.

data QrConfig = QrConfig {
    mode :: Mode,
    version :: Int
}