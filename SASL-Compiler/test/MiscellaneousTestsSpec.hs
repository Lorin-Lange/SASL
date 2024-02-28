{-|
Module      : MiscellaneousTestsSpec
Description : MiscellaneousTestsSpec
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains miscellaneous tests.
-}

module MiscellaneousTestsSpec where

import Test.HUnit ( (@?=) )
import Test.Hspec ( Spec, describe, it )
import Compiler (runCompiler)

mc91Of42 :: String
mc91Of42 = " def mc91 n = if n > 100 then n - 10 else mc91 (mc91 (n + 11)) . mc91 42"

mc91OfMinus100 :: String
mc91OfMinus100 = " def mc91 n = if n > 100 then n - 10 else mc91 (mc91 (n + 11)) . mc91 (-100)"

mc91Of105 :: String
mc91Of105 = " def mc91 n = if n > 100 then n - 10 else mc91 (mc91 (n + 11)) . mc91 105"

spec :: Spec
spec = do

  describe "mc91 105" $ do
    it "McCarthy 91 function of 105" $ do
      runCompiler mc91Of105 @?= "95"

  describe "mc91 -100" $ do
    it "McCarthy 91 function of -100" $ do
      runCompiler mc91OfMinus100 @?= "91"

  describe "mc91 42" $ do
    it "McCarthy 91 function of 42" $ do
      runCompiler mc91Of42 @?= "91"