{-|
Module      : Utilities.StateTestsSpec
Description : StateTestsSpec
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains tests of the functions 
regarding the state monad.
-}

module Utilities.StateTestsSpec where

import Test.HUnit ( (@?=) )
import Test.Hspec ( Spec, describe, it )

import qualified Data.Map as M ( fromList )
import Control.Monad.State ( execState )
import Control.Monad.State.Lazy ( evalState )

import Utilities.State ( StateI(..), pop, push, insertNode, initState, replaceNode )
import Utilities.Types ( Pointer(..), Builtin (..), SASL (Builtin) )

spec :: Spec
spec = do

  describe "stack 0: push" $ do
    it "pushes a pointer to the stack" $ do
      let st = execState (push $ Pointer 0) initState
      stack st @?= [Pointer 0]

  describe "stack 1: pop" $ do
    it "pops a pointer from the stack" $ do
      let st = initState { stack = [Pointer 1, Pointer 0] }
      stack (execState pop st) @?= [Pointer 0]
      evalState pop st @?= Pointer 1

  describe "insertNode" $ do
    it "inserts a node in the graph" $ do
      let st = execState (insertNode $ Builtin Add) initState
      graph st @?= M.fromList [(Pointer 0, Builtin Add)]
      pointer st @?= Pointer 1

  describe "replaceNode" $ do
    it "replaceNode in graph" $ do
      let st1 = initState { pointer = Pointer 1
                          , graph   = M.fromList [(Pointer 0, Builtin Add)] 
                          }
      let st2 = execState (replaceNode (Pointer 0) (Builtin Sub)) st1
      graph st2 @?= M.fromList [ (Pointer 0, Builtin Sub) ]
      pointer st2 @?= Pointer 1