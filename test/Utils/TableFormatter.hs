module Utils.TableFormatter (renderPipeTable) where

import Data.List (transpose, intercalate)
import Text.Printf (printf)

renderPipeTable :: [String] -> [[String]] -> String
renderPipeTable headers rows =
    intercalate "\n" [ formatRow paddedHeaders
                     , formatSeparator columnWidths
                     , intercalate "\n" (map formatRow paddedRows)
                     ]
  where
    allRows = headers : rows
    columnWidths = map (maximum . map length) (transpose allRows)
    
    padRow = zipWith (printf " %-*s ") columnWidths
    paddedHeaders = padRow headers
    paddedRows    = map padRow rows
    
    formatRow cells = "|" ++ intercalate "|" cells ++ "|"
    formatSeparator widths = "|" ++ intercalate "|" (map (\w -> replicate (w + 2) '-') widths) ++ "|"