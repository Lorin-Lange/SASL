{-|
Module      : CustomLanguageFeaturesTestsSpec
Description : Tests of custom language features
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains unit tests of
custom language features.
-}

module CustomLanguageFeaturesTestsSpec where

import Test.HUnit ( (@?=) )
import Test.Hspec ( Spec, describe, it )
import Compiler (runCompiler)

spec :: Spec
spec = do

  describe "Test a lambda" $ do
    it "lambda: (\\ a b . a + b)" $ do
      runCompiler " . (\\ a b . a + b) 1 2" @?= "3"

  describe "Test chained lambdas" $ do
    it "chained lambdas: (\\ a . (\\ b . a + b))" $ do
      runCompiler " . (\\ a . (\\ b . a + b)) 1 2" @?= "3"

  describe "Test of binding a lambda" $ do
    it "def f = (\\ a b . a + b)" $ do
      runCompiler "def f = (\\ a b . a + b) . f 1 2" @?= "3"

  describe "Test ' operator" $ do
    it "(\\ x . x + 1) ' (\\ x . 2 * x)" $ do
      runCompiler " . (\\x . x + 1) ' (\\ x . 2 * x) ' (\\ x . x + 1) $ 10" @?= "23"

  describe "Test $ operator" $ do
    it "(\\ x . 2 * x) $ (\\ x . x + 1)" $ do
      runCompiler " . (\\x.x + 1) $ (\\x.2 * x) $ (\\x.x + 1) 10" @?= "23"

  describe "Test the laziness" $ do
    it "length ' take 1000 ' repeat $ 1 / 0" $ do
      runCompiler " . length ' take 1000 ' repeat $ 1 / 0" @?= "1000"

  describe "Test single-line comments" $ do
    it "// comment" $ do
      runCompiler "// dsofjsdofjop \n . 4 " @?= "4"

  describe "Test multiline comments" $ do
    it "/* comment */" $ do
      runCompiler "/* sdfs  \n dfsdf */ . 4 " @?= "4"