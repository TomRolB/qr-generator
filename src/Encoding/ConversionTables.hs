{-# LANGUAGE NamedFieldPuns #-}

module Encoding.ConversionTables where

import Control.Monad.Trans.Except (Except, throwE)
import qualified Data.Map as Map
import Shared.Error (AppError (WrongMode, char, mode))
import Shared.Model (Mode (Alphanumeric))

alphanumericTable :: Map.Map Char Int
alphanumericTable = Map.fromList $ zip "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:" [0 ..]

toAlphanumericCode :: Char -> Except AppError Int
toAlphanumericCode char = case Map.lookup char alphanumericTable of
  Nothing -> throwE WrongMode {char, mode = Alphanumeric}
  Just encoding -> return encoding
