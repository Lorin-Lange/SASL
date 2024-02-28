{-|
Module      : Backend.Printer
Description : Printer
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO()

-}

{-# LANGUAGE PatternSynonyms #-}

module Backend.Printer where

import Utilities.State ( pop, StateI, St )
import Utilities.Types (
  pattern BBool,
  pattern BInt,
  pattern BNil,
  pattern BString,
  SASL(Pair),
  Pointer )
import Backend.ReductionMachine (strict)
import Control.Monad.State (evalState)

-- | Pop the result from the stack, fully reduce it and return its String representation.
getResult :: StateI -> String
getResult = evalState (pop >>= showResult)

-- | String representation of a fully reduced expression.
-- Assumes that the latter reduces a printable value.
-- Lazyness ensures that infinite lists are also printed incrementally.
showResult :: Pointer -> St String
showResult ptr = do
  result <- strict ptr
  case result of
    BInt x     -> return $ show x
    BBool b    -> return $ if b then "true" else "false"
    BString s  -> return $ show s
    BNil       -> return "[]"
    Pair hd tl -> do hdString <- showResult hd
                     tlString <- showListResult tl
                     return $ "[" ++ hdString ++ tlString ++ "]"
    _          -> undefined

-- | String representation of fully reduced tail of list.
-- That is: @,x1,x2,x2...@
showListResult :: Pointer -> St String
showListResult ptr = do
  result <- strict ptr
  case result of
    BNil       -> return ""
    Pair hd tl -> do hdString <- showResult hd
                     tlString <- showListResult tl
                     return $ "," ++ hdString ++ tlString
    _          -> error "Tail of list must be another list."