module Shared.Error where
import Shared.Model (Mode)

data AppError = WrongMode { char :: Char, mode :: Mode }