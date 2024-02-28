{-|
Module      : Backend.Optimizer
Description : Optimizer
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO() : Longer description
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend.Optimizer where

import Utilities.State
    ( clearFlags,
      flag,
      flagged,
      getNode,
      insertNode,
      replaceNode,
      stepOptimization,
      St, 
      setOptimizerGraph,
      garbageCollect)
import Utilities.Types
    ( pattern BB,
      pattern BBs,
      pattern BC,
      pattern BCp,
      pattern BSp,
      pattern BI,
      pattern BK,
      Builtin(B, I, K, S),
      Pointer,
      SASL((:@:), Builtin) )
import Control.Monad.Identity ( Identity )

instance MonadFail Identity where
  fail = error


-- * Patterns

-- | Simple pattern type to help check for optimizable structures.
data Pattern = Apply Pattern Pattern -- ^ Function application.
             | Simple Builtin        -- ^ Terminals (Builtins)
             | Any                   -- ^ Anyting.

-- | Match pattern on graph, at specific pointer.
match :: Pattern -> Pointer -> St Bool
match Any _    = return True
match pat expr = do
  token <- getNode expr
  case (pat, token) of
    (Simple x, Builtin y) -> return $ x == y
    (Apply l r, f :@: a)  -> do lMatch <- match l f
                                rMatch <- match r a
                                return $ lMatch && rMatch
    _                     -> return False

-- | Seven patterns to optimize.
pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7 :: Pattern

pattern1 = Apply (Apply (Simple S)
                        (Apply (Simple K) Any))
                 (Apply (Simple K) Any)

pattern2 = Apply (Apply (Simple S)
                        (Apply (Simple K) Any))
                 (Simple I)

pattern3 = Apply (Apply (Simple S)
                        (Apply (Simple K) Any))
                 (Apply (Apply (Simple B) Any) Any)

pattern4 = Apply (Apply (Simple S)
                        (Apply (Simple K) Any))
                 Any

pattern5 = Apply (Apply (Simple S)
                        (Apply (Apply (Simple B) Any) Any))
                 (Apply (Simple K) Any)

pattern6 = Apply (Apply (Simple S) Any) (Apply (Simple K) Any)

pattern7 = Apply (Apply (Simple S)
                        (Apply (Apply (Simple B) Any) Any))
                 Any


-- -* Optimization Rules

-- | @S (K f) (K g)   --->   K (f g)@
rule1 :: Pointer -> St Pointer
rule1 expr = do
  app1 :@: arg2 <- getNode expr
  _    :@: arg1 <- getNode app1

  _ :@: f <- getNode arg1
  _ :@: g <- getNode arg2
 
  k <- insertNode BK
  right <- insertNode $ f :@: g
  replaceNode expr $ k :@: right

-- | @S (K f) I   --->   I f@
rule2 :: Pointer -> St Pointer
rule2 expr = do
  app1 :@: _    <- getNode expr
  _    :@: arg1 <- getNode app1

  _ :@: f <- getNode arg1
 
  i <- insertNode BI
  replaceNode expr $ i :@: f

-- | @S (K f) (B g h)   --->   B* f g h@
rule3 :: Pointer -> St Pointer
rule3 expr = do
  app1 :@: arg2 <- getNode expr
  _    :@: arg1 <- getNode app1

  _  :@: f <- getNode arg1
  bg :@: h <- getNode arg2
  _  :@: g <- getNode bg

  bs    <- insertNode BBs
  left' <- insertNode $ bs :@: f
  left  <- insertNode $ left' :@: g
  replaceNode expr $ left :@: h

-- | @S (K f) g   --->   B f g@
rule4 :: Pointer -> St Pointer
rule4 expr = do
  app1 :@: g    <- getNode expr
  _    :@: arg1 <- getNode app1

  _ :@: f <- getNode arg1

  b    <- insertNode BB
  left <- insertNode $ b :@: f
  replaceNode expr $ left :@: g

-- | @S (B f g) (K h)   --->   C' f g h@
rule5 :: Pointer -> St Pointer
rule5 expr = do
  app1 :@: arg2 <- getNode expr
  _    :@: arg1 <- getNode app1

  _  :@: h <- getNode arg2
  bf :@: g <- getNode arg1
  _  :@: f <- getNode bf

  c'    <- insertNode BCp
  left' <- insertNode $ c' :@: f
  left  <- insertNode $ left' :@: g
  replaceNode expr $ left :@: h

-- | @S f (K g)   --->   C f g@
rule6 :: Pointer -> St Pointer
rule6 expr = do
  app1 :@: arg2 <- getNode expr
  _    :@: f    <- getNode app1

  _ :@: g <- getNode arg2

  c    <- insertNode BC
  left <- insertNode $ c :@: f
  replaceNode expr $ left :@: g

-- | @S (B f g) h   --->   S' f g h@
rule7 :: Pointer -> St Pointer
rule7 expr = do
  app1 :@: h    <- getNode expr
  _    :@: arg1 <- getNode app1

  bf :@: g <- getNode arg1
  _  :@: f <- getNode bf

  s'    <- insertNode BSp
  left' <- insertNode $ s' :@: f
  left  <- insertNode $ left' :@: g
  replaceNode expr $ left :@: h

-- * The Optimizer

-- | All @(pattern, rule)@ pairs.
rules :: [ (Pattern, Pointer -> St Pointer) ]
rules = [ (pattern1, rule1)
        , (pattern2, rule2)
        , (pattern3, rule3)
        , (pattern4, rule4)
        , (pattern5, rule5)
        , (pattern6, rule6)
        , (pattern7, rule7)
        ]

-- | Apply a rule if the provided pattern matches.
applyRule :: Pattern -> (Pointer -> St Pointer) -> Pointer -> St Pointer
applyRule p r expr = do
  m <- match p expr
  if m then stepOptimization >> r expr else return expr

-- | Apply one of the 7 seven rules, if its corresponding pattern matches.
applyRules :: Pointer -> St Pointer
applyRules expr = foldl fld (return expr) rules
  where fld acc (p, r) = acc >>= applyRule p r

-- | Do the optimizing at a specific pointer, bottom up.
-- @optimize@ is a wrapper for @optimizeAt@ that clears all the flags before calling the latter.
-- It also saves the resulting optimization-graph, and collects garbage.
optimize :: Pointer -> St Pointer
optimize expr = do
  _   <- clearFlags
  ptr <- optimizeAt expr >>= garbageCollect
  _   <- setOptimizerGraph
  return ptr

optimizeAt :: Pointer -> St Pointer
optimizeAt expr = do
  flg <- flagged expr
  if flg then return expr else do
    _     <- flag expr
    token <- getNode expr
    case token of
      f :@: a -> do _ <- optimizeAt f
                    _ <- optimizeAt a
                    applyRules expr
      _       -> return expr