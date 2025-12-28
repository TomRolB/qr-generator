{-# LANGUAGE NamedFieldPuns #-}

module Shared.Error where
    
import Shared.Model (Mode)

data AppError = WrongMode { char :: Char, mode :: Mode }

renderError :: AppError -> String
renderError err = case err of
    WrongMode { char, mode }  -> "The character '" ++ [char] ++ "' cannot be encoded in " ++ show mode ++ " mode."