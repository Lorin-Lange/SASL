{-|
Module      : Utilities.Types
Description : Basic Types
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains the basic types used within
the state monad from Utilities.State.
-}

{-# LANGUAGE PatternSynonyms #-}

module Utilities.Types where

import qualified Data.Map as M ( Map )
import qualified Data.Set as S ( Set )

newtype Pointer = Pointer { deref :: Integer }
  deriving (Show, Eq, Ord)

nullPtr :: Pointer
nullPtr = Pointer $ -1

type Stack = [Pointer]
type Graph = M.Map Pointer SASL
type Flags = S.Set Pointer

-- | SASL-Token Type.
data SASL = Pointer :@: Pointer                    -- ^ Function-application.
          | Def [([String], Pointer)] Pointer Bool -- ^ Definitions (Flag is set iff global).
          | Builtin Builtin                        -- ^ Builtin values and functions.
          | Pair Pointer Pointer                   -- ^ Pair node, used for reduction of lists.
  deriving (Show, Eq)

-- | Builting Functions and Values.
data Builtin = Ref String
             | ILit Integer | BLit Bool | SLit String
             | Add | Sub | Mul | Div 
             | Plus | Minus
             | And | Or | Not
             | Eq | Nq | Lt | Lq | Gt | Gq
             | Hd | Tl | Cons | Cond
             | Nil
             | S | K | I | Y | U
             | B | C | Sp | Bs | Cp
  deriving (Show, Eq)

pattern BRef :: String -> SASL
pattern BRef s = Builtin (Ref s)

pattern BInt :: Integer -> SASL
pattern BInt a = Builtin (ILit a)

pattern BBool :: Bool -> SASL
pattern BBool a = Builtin (BLit a)

pattern BString :: String -> SASL
pattern BString a = Builtin (SLit a)

pattern BCond :: SASL
pattern BCond = Builtin Cond

pattern BK :: SASL
pattern BK = Builtin K

pattern BS :: SASL
pattern BS = Builtin S

pattern BHd :: SASL
pattern BHd = Builtin Hd

pattern BTl :: SASL
pattern BTl = Builtin Tl

pattern BCons :: SASL
pattern BCons = Builtin Cons

pattern BNil :: SASL
pattern BNil = Builtin Nil

pattern BI :: SASL
pattern BI = Builtin I

pattern BY :: SASL
pattern BY = Builtin Y

pattern BU :: SASL
pattern BU = Builtin U

pattern BAdd :: SASL
pattern BAdd = Builtin Add

pattern BSub :: SASL
pattern BSub = Builtin Sub

pattern BMul :: SASL
pattern BMul = Builtin Mul

pattern BDiv :: SASL
pattern BDiv = Builtin Div

pattern BAnd :: SASL
pattern BAnd = Builtin And

pattern BOr :: SASL
pattern BOr = Builtin Or

pattern BEq :: SASL
pattern BEq = Builtin Eq

pattern BNq :: SASL
pattern BNq = Builtin Nq

pattern BLt :: SASL
pattern BLt = Builtin Lt

pattern BLq :: SASL
pattern BLq = Builtin Lq

pattern BGt :: SASL
pattern BGt = Builtin Gt

pattern BGq :: SASL
pattern BGq = Builtin Gq

pattern BPlus :: SASL
pattern BPlus = Builtin Plus

pattern BMinus :: SASL
pattern BMinus = Builtin Minus

pattern BNot :: SASL
pattern BNot = Builtin Not

pattern BB :: SASL
pattern BB = Builtin B

pattern BC :: SASL
pattern BC = Builtin C

pattern BSp :: SASL
pattern BSp = Builtin Sp

pattern BBs :: SASL
pattern BBs = Builtin Bs

pattern BCp :: SASL
pattern BCp = Builtin Cp