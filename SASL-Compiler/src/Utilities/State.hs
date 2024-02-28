{-|
Module      : Utilities.State
Description : State
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains the state monad and its
utility functions which are used by other modules.
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Utilities.State where

import Utilities.Types ( SASL(..), Flags, Graph, Stack, Pointer(..), nullPtr )
import Control.Monad.State ( gets, MonadState(put, get), State )
import qualified Data.Map as M ( insert, lookup, empty )
import qualified Data.Set as S ( insert, member, empty )
import Data.Maybe (fromJust)
import Data.List (intercalate)

import Prelude hiding (traverse)

-- | State transformer type for the state monad.
type St = State StateI

-- | Data type for the state monad.
data StateI = StateI { pointer :: Pointer            -- ^ The next available pointer in the graph.
                     , graph   :: Graph              -- ^ The program-graph.
                     , stack   :: Stack              -- ^ The stack used during reduction.
                     , flags   :: Flags              -- ^ Set of flags used for graph travsersals.
                     , abstractionFlags  :: Flags    -- ^ Other set of flags, used for abstracting in optimized compiler.
                     , reductionSteps    :: Integer  -- ^ Number of performed reduction-steps.
                     , optimizationSteps :: Integer  -- ^ Number of performed optimization-steps.

                     , parserGraph    :: Graph -- ^ Graph after parsing.
                     , compilerGraph  :: Graph -- ^ Graph after compiling.
                     , optimizerGraph :: Graph -- ^ Graph after optimizing.
                     , reductionGraph :: Graph -- ^ Graph after reduction.
                     }
  deriving (Show, Eq)

-- | Initial State passed to Parser.
initState :: StateI
initState = StateI { pointer = Pointer 0
                   , graph   = M.empty
                   , stack   = []
                   , flags   = S.empty
                   , abstractionFlags  = S.empty
                   , reductionSteps    = 0
                   , optimizationSteps = 0

                   , parserGraph    = M.empty
                   , compilerGraph  = M.empty
                   , optimizerGraph = M.empty
                   , reductionGraph = M.empty
                   }


-- * General

-- | Print state summary.
printState :: StateI -> IO ()
printState state = do
  putStr "Optimization steps: "
  print $ optimizationSteps state
  putStr "Reduction steps: "
  print $ reductionSteps state

-- | Increment reduction-step counter.
stepReduction :: St ()
stepReduction = do
  state <- get
  put $ state { reductionSteps = reductionSteps state + 1 }

-- | Increment optimization-step counter.
stepOptimization :: St ()
stepOptimization = do
  state <- get
  put $ state { optimizationSteps = optimizationSteps state + 1 }

-- | Set parserGraph to current graph.
setParserGraph :: St ()
setParserGraph = do
  state <- get
  put $ state { parserGraph = graph state }

-- | Set compilerGraph to current graph.
setCompilerGraph :: St ()
setCompilerGraph = do
  state <- get
  put $ state { compilerGraph = graph state }

-- | Set optimizerGraph to current graph.
setOptimizerGraph :: St ()
setOptimizerGraph = do
  state <- get
  put $ state { optimizerGraph = graph state }

-- | Set reductionGraph to current graph.
setReductionGraph :: St ()
setReductionGraph = do
  state <- get
  put $ state { reductionGraph = graph state }

-- | Get the saved graphs.
getGraphs :: St (Graph, Graph, Graph, Graph)
getGraphs = do
  g1 <- gets parserGraph
  g2 <- gets compilerGraph
  g3 <- gets optimizerGraph
  g4 <- gets reductionGraph
  return (g1, g2, g3, g4)

-- * Stack Utils

-- | Pushes a pointer to the stack and returns it.
push :: Pointer -> St Pointer
push ptr = do
  state <- get
  put $ state { stack = ptr : stack state }
  return ptr

-- | Type alias for error messages.
type ErrorMessage = String

-- | Takes the first pointer from the stack and returns it.
pop :: St Pointer
pop = do
  state <- get
  let s = stack state
  put state { stack = tailWithError errorMessage s }
  return $ headWithError errorMessage s
 where errorMessage = "Called pop on empty stack."

-- | Head with custom error message.
headWithError :: ErrorMessage -> [a] -> a
headWithError err []  = error err
headWithError _ (x:_) = x

-- | Tail with custom error message.
tailWithError :: ErrorMessage -> [a] -> [a]
tailWithError err []   = error err
tailWithError _ (_:xs) = xs


-- * Flagging Utils.

-- | Flag pointer, return nothing.
flag :: Pointer -> St ()
flag p = do
  state <- get
  let f = flags state
  put $ state { flags = S.insert p f }

-- | Check if pointer is flagged.
flagged :: Pointer -> St Bool
flagged p = gets $ S.member p . flags

-- | Clear all flags and return nothing.
clearFlags :: St ()
clearFlags = do
  state <- get
  put $ state { flags = S.empty }

-- ** Flagging Utils, specifically for abstractionFlags.

-- | Flag pointer, return nothing.
abstractionFlag :: Pointer -> St ()
abstractionFlag p = do
  state <- get
  let f = abstractionFlags state
  put $ state { abstractionFlags = S.insert p f }

-- | Check if pointer is flagged.
abstractionFlagged :: Pointer -> St Bool
abstractionFlagged p = gets $ S.member p . abstractionFlags

-- | Clear all flags and return nothing.
clearAbstractionFlags :: St ()
clearAbstractionFlags = do
  state <- get
  put $ state { abstractionFlags = S.empty }


-- * Program-Graph Utils

-- | Insert node at next available Pointer, return & increment the latter.
insertNode :: SASL -> St Pointer
insertNode tk = do
  state <- get
  let p = pointer state
  put state { pointer = Pointer $ deref p + 1
            , graph   = M.insert p tk $ graph state
            }
  return p

-- | Insert node at given pointer and return the latter.
replaceNode :: Pointer -> SASL -> St Pointer
replaceNode p tk = do
  state <- get
  put state { graph = M.insert p tk $ graph state }
  return p

-- | Get node at pointer.
getNode :: Pointer -> St SASL
getNode p = gets $ fromJust . M.lookup p . graph


-- * Garbage-Collection

-- | Garbage collector: traverses graph from given pointer, collects
-- connected component and drops the rest.
garbageCollect :: Pointer -> St Pointer
garbageCollect root = do
  state <- get
  let g = graph state
  put $ state { graph = M.empty }
  clearFlags >> traverse root g
 where
  traverse root g = do
    flg <- flagged root
    if flg then return root else do
      _ <- flag root
      let token = fromJust $ M.lookup root g
      case token of
        f :@: a    -> do _ <- traverse f g
                         _ <- traverse a g
                         replaceNode root token
        Pair h t   -> do _ <- traverse h g
                         _ <- traverse t g
                         replaceNode root token
        Def ds e _ -> do _ <- traverse e g
                         _ <- traverseDefs ds g
                         replaceNode root token
        _          -> replaceNode root token

  traverseDefs [] _ = return ()
  traverseDefs ( ( _ , def ) : rest ) g = do
    _ <- traverseDefs rest g
    _ <- traverse def g
    return ()


-- * REPL Utils

-- | Check if a given program is executable.
executable :: Pointer -> St Bool
executable prog
  | prog == nullPtr = return False
  | otherwise       = do
      token <- getNode prog
      case token of
        Def _ e True -> return $ e /= nullPtr
        _            -> return True

-- | Merge AST into another AST, all in the same graph (merge the second into the first).
merge :: Pointer -> Pointer -> St Pointer
merge p1 p2
  | p1 == nullPtr = return p2
  | p2 == nullPtr = return p1
  | otherwise     = do
      token1 <- getNode p1
      token2 <- getNode p2
      case (token1, token2) of
        (Def ds1 _ True, Def ds2 e True) -> replaceNode p1 $ Def (mergeDefs ds2 ds1) e True
        (Def ds _ True,  _)              -> replaceNode p1 $ Def ds p2 True
        _                                -> error "Can't merge these ASTs."

-- | Merge Def-lists (take union with resp. to idfr, prioritize the first one).
mergeDefs :: [ ( [String] , Pointer ) ] -> [ ( [String] , Pointer ) ] -> [ ( [String] , Pointer ) ]
mergeDefs ds1 ds2 = foldr insertDef ds2 ds1
  where insertDef d [] = [d]
        insertDef d1@( idfr1 : _ , _ ) ( d2@( idfr2 : _ , _ ) : rest )
          | idfr1 == idfr2 = d1 : rest
          | otherwise      = d2 : insertDef d1 rest
        insertDef _ _ = undefined

-- | Set Program null, but keep global definitions.
clearProg :: Pointer -> St Pointer
clearProg prog = do
  token <- getNode prog
  case token of
    Def ds _ True -> replaceNode prog $ Def ds nullPtr True
    _             -> return nullPtr

-- | Print summary of all the currently defined functions.
-- Consider: Generally, duplicates are possible.
printDefinitions :: Pointer -> StateI -> IO ()
printDefinitions ptr state
  | ptr == nullPtr = putStrLn "\nNone\n"
  | otherwise      = do
      putStrLn "\nCurrently defined:\n"
      let token = fromJust $ M.lookup ptr $ graph state
      case token of
        Def ds _ True -> do _ <- putStrLn $ intercalate "\n" $ map (head . fst) ds
                            putStrLn ""
        _             -> putStrLn "None\n"