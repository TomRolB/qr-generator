module Utils.MapUtils where

import qualified Data.Map as Map

invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
invertMap m = Map.fromList [(v, k) | (k, v) <- Map.toList m]
