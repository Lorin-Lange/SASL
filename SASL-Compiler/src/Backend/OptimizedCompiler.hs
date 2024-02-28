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

module Backend.OptimizedCompiler where

import Backend.Compiler (replaceAllDefs, doLess)

import Utilities.State ( 
  clearFlags, 
  flag,
  flagged,
  clearAbstractionFlags,
  abstractionFlag,
  abstractionFlagged,
  getNode, 
  insertNode, 
  replaceNode,
  St,
  setCompilerGraph,
  garbageCollect )

import Utilities.Types
    ( pattern BRef,
      pattern BS,
      pattern BI, 
      Pointer,
      SASL((:@:), 
      Def) )


-- * Abstracting

-- | Abstract identifier in an expression.
-- @abstract@ is a wrapper for @doAbstract@, that clears all abstraction-flags before calling the latter.
abstract :: String -> Pointer -> St Pointer
abstract idfr expr = clearAbstractionFlags >> doAbstract idfr expr

doAbstract :: String -> Pointer -> St Pointer
doAbstract idfr expr = do
  flg <- abstractionFlagged expr
  if flg then return expr else do
    fnd <- abstractFind idfr expr
    _   <- abstractionFlag expr
    if not fnd then doLess expr else do
      token <- getNode expr
      case token of
        BRef _  -> replaceNode expr BI
        f :@: a -> abstractApp idfr expr f a
        _       -> undefined

-- | Abstract identifier in a function application.
abstractApp :: String -> Pointer -> Pointer -> Pointer -> St Pointer
abstractApp idfr expr f a = do
  _    <- doAbstract idfr f
  _    <- doAbstract idfr a
  s    <- insertNode BS
  left <- insertNode $ s :@: f
  replaceNode expr $ left :@: a

-- | Abstract list of identifiers in an expression, starting from the right.
abstractArgs :: [String] -> Pointer -> St Pointer
abstractArgs args def = foldr abstractArg (return def) args
  where abstractArg arg acc = acc >>= abstract arg

-- | Check if an identifier occurs in an expression, or if it has already
-- been abstracted in one of the latters subexpressions.
-- @abstractFind@ is a wrapper for @doAbstractFind@, that clears all flags before calling the latter.
abstractFind :: String -> Pointer -> St Bool
abstractFind idfr expr = clearFlags >> doAbstractFind idfr expr

doAbstractFind :: String -> Pointer -> St Bool
doAbstractFind idfr expr = do
  flg <- flagged expr
  if flg then return False else do
    _     <- flag expr
    token <- getNode expr
    case token of
      BRef y  -> return $ idfr == y
      f :@: a -> do fFind <- doAbstractFind idfr f
                    aFind <- doAbstractFind idfr a
                    aflg  <- abstractionFlagged expr
                    return $ aflg || fFind || aFind
      _       -> abstractionFlagged expr


-- * Definitions (global and local)

-- | Compile definitions. Works just like @global@, but uses the new abstraction method defined above.
compileDefs :: [ ( [String] , Pointer ) ] -> Pointer -> St Pointer
compileDefs ds scope = do
  compiledDefs <- compileAllDefs $ zip allArgs allDefs
  iterate (replaceAllDefs $ zip allIdfrs compiledDefs) (return scope) !! (length ds + 1)
 where
  allIdfrs = [ idfr | (idfr : _ , _) <- ds ]
  allArgs  = [ args | (_ : args , _) <- ds ]
  allDefs  = [ def  | (_ : _ , def)  <- ds ]

-- | List of Pointers to all compiled definitions.
compileAllDefs :: [ ( [String] , Pointer ) ] -> St [ Pointer ]
compileAllDefs [] = return []
compileAllDefs ( ( args , def ) : rest ) = do
  tl <- compileAllDefs rest
  hd <- compileAt def >>= abstractArgs args
  return $ hd : tl


-- * Compiling

-- | Compile parsed Program at a specific pointer.
-- @optimizedCompile@ is a wrapper for @compileAt@, that saves the resulting compilation-graph,
-- and collects garbage.
optimizedCompile :: Pointer -> St Pointer
optimizedCompile root = do
  ptr <- compileAt root >>= garbageCollect
  _   <- setCompilerGraph
  return ptr
  
compileAt :: Pointer -> St Pointer
compileAt root = do
  token <- getNode root
  case token of
    Def ds e _ -> compileAt e >>= compileDefs ds
    f :@: a    -> do fCompiled <- compileAt f
                     aCompiled <- compileAt a
                     replaceNode root $ fCompiled :@: aCompiled
    _          -> return root