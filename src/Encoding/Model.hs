module Encoding.Model (Bit (..), AppM) where

import Control.Monad.Trans.Except (Except)
import Shared.Error (AppError)

type AppM a = Except AppError a

data Bit = Zero | One
