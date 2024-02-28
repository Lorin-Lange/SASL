{-|
Module      : Backend.ReductionMachine
Description : ReductionMachine
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains the SK reduction machine which 
manupulates the graph of the program itself.

-}

{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Backend.ReductionMachine where

import Utilities.State
    ( getNode, insertNode, pop, push, replaceNode, St, stepReduction, setReductionGraph, garbageCollect )

import Utilities.Types
    ( Builtin(Nq, Add, Sub, Mul, Div, Lt, Gt, Lq, Gq, Plus, Minus, And,
              Or, Hd, Tl, Eq),
      SASL(Pair, (:@:)),
      Pointer,
      nullPtr,
      pattern BCp, pattern BBs, pattern BSp, pattern BC, pattern BB,
      pattern BNot, pattern BMinus, pattern BPlus, pattern BGq, pattern BGt,
      pattern BLq, pattern BLt, pattern BNq, pattern BEq, pattern BOr,
      pattern BAnd, pattern BDiv, pattern BMul, pattern BSub, pattern BAdd,
      pattern BU, pattern BY, pattern BI, pattern BCons, pattern BTl,
      pattern BHd, pattern BS, pattern BK, pattern BCond, pattern BBool,
      pattern BInt, pattern BRef )

import Control.Monad.Identity (Identity)

instance MonadFail Identity where
  fail = error


-- * Utilities

-- | Strict evaluation shorthand.
-- Reduces the provided expression, pops the result off the Stack and returns the associated token.
strict :: Pointer -> St SASL
strict arg = reduceAt arg >> pop >>= getNode

-- | Pop an argument from the stack.
popArg :: St Pointer
popArg = do
  top <- pop
  if top == nullPtr then
    error "Not enough input arguments, can't print function."
  else
    return top

-- | Given a positive Integer n, pop n arguments off the stack and return them.
-- Also returns a pointer to the root application.
popArgs :: Integer -> St ( Pointer , [Pointer] )
popArgs n = do
  app       <- popArg
  _ :@: arg <- getNode app
  if n <= 1 then
    return (app, [arg])
  else do
    (app', args) <- popArgs $ n - 1
    return $ (app', arg : args)

-- | Generate type-error message.
typeError :: String -> String -> String -> String
typeError op expect arg = op ++ " expected " ++ expect ++ ", but got " ++ arg


-- * Reducing Combinators

-- | Skip the I-combinator by pushing its argument on the stack.
reduceI :: St Pointer
reduceI = do
  (_, [arg]) <- popArgs 1
  push arg

-- | Reduce K-combinator: @K x y   --->   x@
reduceK :: St Pointer
reduceK = do
  (app, [arg1, _]) <- popArgs 2

  i <- insertNode BI
  _ <- replaceNode app $ i :@: arg1

  push app


-- | Reduce S-combinator: @S f g x   --->   f x (g x)@
reduceS :: St Pointer
reduceS = do
  (app, [arg1, arg2, arg3]) <- popArgs 3

  left  <- insertNode $ arg1 :@: arg3
  right <- insertNode $ arg2 :@: arg3
  _ <- replaceNode app $ left :@: right

  push app

-- | Reduce Y-combinator; realized via self-reference:
-- Y f   --->   f (Y f)
reduceY :: St Pointer
reduceY = do
  (app, [arg]) <- popArgs 1
  _ <- replaceNode app $ arg :@: app
  push app

-- | Reduce U-combinator: @U f z   --->   f (hd z) (tl z)@
reduceU :: St Pointer
reduceU = do
  (app, [arg1, arg2]) <- popArgs 2

  hd <- insertNode BHd
  tl <- insertNode BTl

  left' <- insertNode $ hd :@: arg2
  left  <- insertNode $ arg1 :@: left'
  right <- insertNode $ tl :@: arg2
  _ <- replaceNode app $ left :@: right

  push app

-- | Reduce B-combinator: @B f g x   --->   f (g x)@
reduceB :: St Pointer
reduceB = do
  (app, [arg1, arg2, arg3]) <- popArgs 3

  right <- insertNode $ arg2 :@: arg3
  _ <- replaceNode app $ arg1 :@: right

  push app

-- | Reduce C-combinator: @C f g x   --->   f x g@
reduceC :: St Pointer
reduceC = do
  (app, [arg1, arg2, arg3]) <- popArgs 3

  left <- insertNode $ arg1 :@: arg3
  _ <- replaceNode app $ left :@: arg2

  push app

-- | Reduce S'-combinator: @S' c f g x   --->   c (f x) (g x)@
reduceSp :: St Pointer
reduceSp = do
  (app, [arg1, arg2, arg3, arg4]) <- popArgs 4

  left' <- insertNode $ arg2 :@: arg4
  left  <- insertNode $ arg1 :@: left'
  right <- insertNode $ arg3 :@: arg4
  _ <- replaceNode app $ left :@: right

  push app

-- | Reduce B*-combinator: @B* c f g x   --->   c (f (g x))@
reduceBs :: St Pointer
reduceBs = do
  (app, [arg1, arg2, arg3, arg4]) <- popArgs 4

  right' <- insertNode $ arg3 :@: arg4
  right  <- insertNode $ arg2 :@: right'
  _ <- replaceNode app $ arg1 :@: right

  push app

-- | Reduce C'-combinator: @C' c f g x   --->   c (f x) g@
reduceCp :: St Pointer
reduceCp = do
  (app, [arg1, arg2, arg3, arg4]) <- popArgs 4

  left' <- insertNode $ arg2 :@: arg4
  left  <- insertNode $ arg1 :@: left'
  _ <- replaceNode app $ left :@: arg3

  push app


-- * Reducing builtin functions

-- | Reduce conditional expression.
reduceCond :: St Pointer
reduceCond = do
  (app, [arg1, arg2, arg3]) <- popArgs 3

  condition <- strict arg1
  let result = case condition of
                 BBool True  -> arg2
                 BBool False -> arg3
                 _ -> error $ typeError "Conditional" "BLit" (show condition)

  i <- insertNode BI
  _ <- replaceNode app $ i :@: result

  push app

-- | Reduce cons expression.
reduceCons :: St Pointer
reduceCons = do
  (app, [arg1, arg2]) <- popArgs 2
  _ <- replaceNode app $ Pair arg1 arg2
  push app

-- | Reduce binary arithmetic.
reduceAri :: Builtin -> St Pointer
reduceAri op = do
  (app, [arg1, arg2]) <- popArgs 2
  
  x <- strict arg1
  let v1 = case x of
             BInt a -> a
             _ -> error $ typeError (show op) "ILit" (show x)

  y <- strict arg2
  let v2 = case y of
             BInt b -> b
             _ -> error $ typeError (show op) "ILit" (show y)

  let fn = case op of
             Add -> (+)
             Sub -> (-)
             Mul -> (*)
             Div -> div
             _ -> undefined

  _ <- replaceNode app $ BInt $ fn v1 v2
  push app

-- | Reduce numeric comparison expression.
reduceCom :: Builtin -> St Pointer
reduceCom op = do
  (app, [arg1, arg2]) <- popArgs 2

  x <- strict arg1
  let v1 = case x of
             BInt a -> a
             _ -> error $ typeError (show op) "ILit" (show x)

  y <- strict arg2
  let v2 = case y of
             BInt b -> b
             _ -> error $ typeError (show op) "ILit" (show y)

  let fn = case op of
             Lt -> (<)
             Lq -> (<=)
             Gt -> (>)
             Gq -> (>=)
             _ -> undefined

  _ <- replaceNode app $ BBool $ fn v1 v2
  push app

-- | Reduce unary arithmetic.
reduceUno :: Builtin -> St Pointer
reduceUno op = do
  (app, [arg]) <- popArgs 1

  x <- strict arg
  let v = case x of
            BInt a -> a
            _ -> error $ typeError (show op) "ILit" (show x)

  let fn = case op of
             Plus  -> id
             Minus -> negate
             _ -> undefined

  _ <- replaceNode app $ BInt $ fn v
  push app

-- | Reduce binary logic expression.
reduceLog :: Builtin -> St Pointer
reduceLog op = do
  (app, [arg1, arg2]) <- popArgs 2

  x <- strict arg1
  _ <- case (x, op) of
         (BBool False, And) -> replaceNode app $ BBool False
         (BBool True, And)  -> strict arg2 >>= replaceNode app
         (BBool True, Or)   -> replaceNode app $ BBool True
         (BBool False, Or)  -> strict arg2 >>= replaceNode app
         _ -> error $ typeError (show op) "BLit" (show x)

  push app

-- | Reduce unary logical expression (Not).
reduceNot :: St Pointer
reduceNot = do
  (app, [arg]) <- popArgs 1

  x <- strict arg
  _ <- case x of
         BBool b -> replaceNode app $ BBool $ not b
         _ -> error $ typeError "Not" "BLit" (show x)

  push app

-- | Reduce builtin list function (hd and tl).
reduceLst :: Builtin -> St Pointer
reduceLst op = do
  (app, [arg]) <- popArgs 1

  lst <- strict arg
  let result = case (lst, op) of
                 (Pair hd _, Hd) -> hd
                 (Pair _ tl, Tl) -> tl
                 _ -> error $ typeError (show op) "pair" (show lst)

  i <- insertNode BI
  _ <- replaceNode app $ i :@: result

  push app

-- | Reduce equality expression.
reduceEq :: Builtin -> St Pointer
reduceEq op = do
  (app, [arg1, arg2]) <- popArgs 2

  x <- strict arg1
  y <- strict arg2
  eq <- checkEq x y

  _ <- replaceNode app $ BBool $ if op == Eq then eq else not eq
  push app

-- | Check equality of tokens.
checkEq :: SASL -> SASL -> St Bool
checkEq (Pair hd1 tl1) (Pair hd2 tl2) = do
  v1 <- strict hd1
  v2 <- strict hd2
  headsEq <- checkEq v1 v2
  if not headsEq then return False else do
    v1' <- strict tl1
    v2' <- strict tl2
    checkEq v1' v2'
checkEq x y = return $ x == y


-- * The Reduction-Machine

-- | Do the reduction, using the stack.
doReduce :: St Pointer
doReduce = do
  root  <- pop
  token <- getNode root
  case token of
    f :@: _  -> push root >> push f >> doReduce
    Pair _ _ -> push root
    BRef ref -> error $ "Unresolved reference " ++ show ref

    BI -> applyRule reduceI
    BK -> applyRule reduceK
    BS -> applyRule reduceS
    BY -> applyRule reduceY
    BU -> applyRule reduceU

    BB  -> applyRule reduceB
    BC  -> applyRule reduceC 
    BSp -> applyRule reduceSp
    BBs -> applyRule reduceBs
    BCp -> applyRule reduceCp

    BCond -> applyRule reduceCond
    BCons -> applyRule reduceCons

    BAdd -> applyRule $ reduceAri Add
    BSub -> applyRule $ reduceAri Sub
    BMul -> applyRule $ reduceAri Mul
    BDiv -> applyRule $ reduceAri Div

    BLt -> applyRule $ reduceCom Lt
    BGt -> applyRule $ reduceCom Gt
    BLq -> applyRule $ reduceCom Lq
    BGq -> applyRule $ reduceCom Gq

    BPlus  -> applyRule $ reduceUno Plus
    BMinus -> applyRule $ reduceUno Minus

    BAnd -> applyRule $ reduceLog And
    BOr  -> applyRule $ reduceLog Or
    BNot -> applyRule reduceNot

    BHd -> applyRule $ reduceLst Hd
    BTl -> applyRule $ reduceLst Tl

    BEq -> applyRule $ reduceEq Eq
    BNq -> applyRule $ reduceEq Nq

    _ -> push root

 where applyRule rule = stepReduction >> rule >> doReduce

-- | Do the reduction, starting a specific Pointer.
-- @reduce@ is a wrapper for @reduceAt@, that also saves the resuling reduction-graph,
-- and collects garbage.
reduce :: Pointer -> St Pointer
reduce root = do
  ptr <- reduceAt root >>= garbageCollect
  _   <- setReductionGraph
  return ptr

reduceAt :: Pointer -> St Pointer
reduceAt root = do
  result <- push nullPtr >> push root >> doReduce >> pop
  top    <- pop
  if top == nullPtr then
    push result
  else
    error "Too many input arguments."