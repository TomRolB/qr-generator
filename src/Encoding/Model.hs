module Encoding.Model (Bit(..), AppM) where

import Shared.Model (Mode)
import Shared.Error (AppError)
import Control.Monad.Trans.Except (Except)

type AppM a = Except AppError a
data Bit = Zero | One


