{-|
Module      : Backend.Compiler
Description : Compiler
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO()

-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend.Compiler where

import Utilities.State ( 
  clearFlags, 
  flag, 
  flagged, 
  getNode, 
  insertNode, 
  replaceNode, 
  St,
  setCompilerGraph,
  garbageCollect )

import Utilities.Types
    ( pattern BCons,
      pattern BI,
      pattern BK,
      pattern BNil,
      pattern BRef,
      pattern BS,
      pattern BU,
      pattern BY,
      Pointer,
      SASL((:@:), 
      Def) )
    
import Control.Applicative (liftA2)


-- * Abstracting

-- | Abstract identifier in an expression.
abstract :: String -> Pointer -> St Pointer
abstract idfr expr = do
  fnd <- find idfr expr
  if not fnd then doLess expr else do
    token <- getNode expr
    case token of
      BRef _  -> replaceNode expr BI
      f :@: a -> abstractApp idfr expr f a
      _       -> undefined

-- | Abstract identifier in a constant expression.
doLess :: Pointer -> St Pointer
doLess expr = do
  k     <- insertNode BK
  token <- getNode expr
  copy  <- insertNode token
  replaceNode expr $ k :@: copy

-- | Abstract identifier in a function application.
abstractApp :: String -> Pointer -> Pointer -> Pointer -> St Pointer
abstractApp idfr expr f a = do
  _    <- abstract idfr f
  _    <- abstract idfr a
  s    <- insertNode BS
  left <- insertNode $ s :@: f
  replaceNode expr $ left :@: a

-- | Abstract list of identifiers in an expression, starting from the right.
abstractArgs :: [String] -> Pointer -> St Pointer
abstractArgs args def = foldr abstractArg (return def) args
  where abstractArg arg acc = acc >>= abstract arg


-- * Replacing

-- | Replace the occurences of an identifier, with some given pointer to an expression,
-- in another expression.
-- @replace@ is a wrapper for @doReplace@, that clears all flags before calling the latter.
replace :: String -> Pointer -> Pointer -> St Pointer
replace idfr expr1 expr2 = clearFlags >> doReplace idfr expr1 expr2

doReplace :: String -> Pointer -> Pointer -> St Pointer
doReplace idfr expr1 expr2 = do
  flg <- flagged expr2
  if flg then return expr2 else do
    _     <- flag expr2
    token <- getNode expr2
    case token of
      BRef y  -> replaceRef idfr expr1 expr2 y
      f :@: a -> replaceApp idfr expr1 expr2 f a
      _       -> return expr2

-- | Replace a reference by a given pointer if it matches the provided identifier.
replaceRef :: String -> Pointer -> Pointer -> String -> St Pointer
replaceRef idfr expr1 expr2 y
  | idfr /= y = return expr2
  | otherwise = do
      i <- insertNode BI
      replaceNode expr2 $ i :@: expr1

-- | Replace occurences of an identifier in a function appplication.
replaceApp :: String -> Pointer -> Pointer -> Pointer -> Pointer -> St Pointer
replaceApp idfr expr1 expr2 f a = do
  _ <- doReplace idfr expr1 f
  _ <- doReplace idfr expr1 a
  return expr2


-- * Finding

-- | Check if a given identifier occurs in an expression.
-- @find@ is a wrapper for @doFind@, that clears all flags before calling the latter.
find :: String -> Pointer -> St Bool
find idfr expr = clearFlags >> doFind idfr expr

doFind :: String -> Pointer -> St Bool
doFind idfr expr = do
  flg <- flagged expr
  if flg then return False else do
    _     <- flag expr
    token <- getNode expr
    case token of
      BRef y  -> return $ idfr == y
      f :@: a -> do fFind <- doFind idfr f
                    aFind <- doFind idfr a
                    return $ fFind || aFind
      _       -> return False


-- * Global Definitions

-- | Compile given global definitions, and replace all their occurences, both in
-- each other and the provided scope.
global :: [ ( [String] , Pointer ) ] -> Pointer -> St Pointer
global ds scope = do
  compiledDefs <- compileAllDefs $ zip allArgs allDefs
  iterate (replaceAllDefs $ zip allIdfrs compiledDefs) (return scope) !! (length ds + 1)
 where
  allIdfrs = [ idfr | (idfr : _ , _) <- ds ]
  allArgs  = [ args | (_ : args , _) <- ds ]
  allDefs  = [ def  | (_ : _ , def)  <- ds ]

-- | List of Pointers to all compiled global definitions.
compileAllDefs :: [ ( [String] , Pointer ) ] -> St [ Pointer ]
compileAllDefs [] = return []
compileAllDefs ( ( args , def ) : rest ) = do
  tl <- compileAllDefs rest
  hd <- compileAt def >>= abstractArgs args
  return $ hd : tl

-- | Replace occurences of all given identifiers and their associated definitions,
-- in the compiled scope.
replaceAllDefs :: [ ( String , Pointer ) ] -> St Pointer -> St Pointer
replaceAllDefs [] scope = scope
replaceAllDefs ( ( idfr , def ) : rest ) scope =
  replaceAllDefs rest scope >>= replace idfr def


-- * Local Definitions

-- | Compile a single local definition.
singleLocal :: [ ( [String] , Pointer ) ] -> Pointer -> St Pointer
singleLocal ( ( idfr : args , def ) : _ ) scope = do
  defCompiled <- compileAt def >>= abstractArgs args
  _           <- abstract idfr scope

  recursive <- find idfr defCompiled
  if not recursive then do
    insertNode $ scope :@: defCompiled
  else do
    _     <- abstract idfr defCompiled
    y     <- insertNode BY
    right <- insertNode $ y :@: defCompiled
    insertNode $ scope :@: right

-- | Compile many local definitions.
manyLocal :: [([String], Pointer)] -> Pointer -> St Pointer
manyLocal ds scope = do
  left  <- uChain allIdfrs scope
  right <- allDefsCompiled $ zip allArgs allDefs
  
  recursive <- foldr (liftA2 (||)) (return False) (flip find right <$> allIdfrs)
  if not recursive then do
    insertNode $ left :@: right
  else do
    y      <- insertNode BY
    uRight <- uChain allIdfrs right
    yRight <- insertNode $ y :@: uRight
    insertNode $ left :@: yRight
 where
  allIdfrs = [ idfr | (idfr : _ , _) <- ds ]
  allArgs  = [ args | (_ : args , _) <- ds ]
  allDefs  = [ def  | (_ : _ , def)  <- ds ]

-- | For @[f1,f2,...,fn]@ and e, create: @U [f1](U [f2]( ... (U [fn](K e)...)))@
uChain :: [String] -> Pointer -> St Pointer
uChain [] expr = do
  k <- insertNode BK
  insertNode $ k :@: expr
uChain ( idfr : rest ) expr = do
  right <- uChain rest expr >>= abstract idfr
  u     <- insertNode BU
  insertNode $ u :@: right

-- | For haskell list @[(args1, e1), (args2, e2),...,(argsN, eN)]@ create
-- SASL-list @[[args1]e1, [args2]e1, ..., [argsN]eN]@, where the @ei@ are compiled.
allDefsCompiled :: [ ( [String] , Pointer ) ] -> St Pointer
allDefsCompiled [] = insertNode BNil
allDefsCompiled ( ( args , def ) : rest ) = do
  tl   <- allDefsCompiled rest
  hd   <- compileAt def >>= abstractArgs args
  cons <- insertNode BCons
  left <- insertNode $ cons :@: hd
  insertNode $ left :@: tl


-- * Compiling

-- | Compile parsed program at specific pointer.
-- @compile@ is a wrapper for @compileAt@, that saves the resulting compilation-graph,
-- and collects garbage.
compile :: Pointer -> St Pointer
compile root = do
  ptr <- compileAt root >>= garbageCollect
  _   <- setCompilerGraph
  return ptr

compileAt :: Pointer -> St Pointer
compileAt root = do
  token <- getNode root
  case token of
    Def ds     e True  -> compileAt e >>= global ds
    Def ds@[_] e False -> compileAt e >>= singleLocal ds
    Def ds     e False -> compileAt e >>= manyLocal ds
    f :@: a            -> do fCompiled <- compileAt f
                             aCompiled <- compileAt a
                             replaceNode root $ fCompiled :@: aCompiled
    _                  -> return root