{-|
Module      : SASLPreludeTestsSpec
Description : Tests of the SASLPrelude
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains unit tests (HSpec) of the 
Prelude, i. e. our standard library.
-}

module SASLPreludeTestsSpec where

import Test.HUnit ( (@?=) )
import Test.Hspec ( Spec, describe, it )
import Compiler (runCompiler)
import SASLPrelude (idTestRes0, idTest0, idTestRes1, idTest1, idTestRes2, idTest2, mapTest0, mapTestRes0, mapTestRes1, mapTest1, mapTest2, mapTestRes2, foldTestRes0, 
  foldTest0, foldTest1, foldTestRes1, foldTestRes2, foldTest2, plusTest0, plusTestRes0, plusTestRes1, plusTest1, plusTestRes2, plusTest2, sumTest0, sumTestRes0, sumTest1, 
  sumTestRes1, sumTest2, sumTestRes2, productTest0, productTestRes0, productTest1, productTestRes1, productTest2, productTestRes2, mulTest0, mulTestRes0, mulTest1, 
  mulTestRes1, mulTest2, mulTestRes2, takeWhileTest0, takeWhileTestRes0, takeWhileTestRes1, takeWhileTest1, takeWhileTest2, takeWhileTestRes2, lengthTest0, lengthTestRes0, 
  lengthTest1, lengthTestRes1, lengthTest2, lengthTestRes2, untilTest0, untilTestRes0, untilTest1, untilTestRes2, untilTestRes1, untilTest2, compTestRes0, compTest0, 
  compTestRes1, compTest1, compTest2, compTestRes2, appendTest0, appendTestRes0, appendTestRes2, appendTestRes1, appendTest1, appendTest2, reverseTestRes2, reverseTestRes1, 
  reverseTestRes0, reverseTest0, reverseTest1, reverseTest2, filterTest0, filterTestRes0, sortTestRes1, sortTestRes0, sortTestRes2, sortTest2, sortTest1, sortTest0, 
  filterTest1, filterTestRes1, filterTestRes2, filterTest2, dropTest0, dropTestRes0, dropTest1, dropTestRes1, dropTestRes2, dropTest2, atTest0, atTestRes0, atTestRes1, 
  atTest1, atTestRes2, atTest2, nullTest0, nullTestRes0, nullTestRes1, nullTest1, nullTestRes2, nullTest2, initTest0, initTestRes0, initTestRes1, initTest1, initTestRes2, 
  initTest2, iterateTestRes1, splitAtTest0, splitAtTestRes0, splitAtTestRes1, splitAtTest1, splitAtTest2, splitAtTestRes2, cycleTest2, cycleTestRes2, cycleTestRes1, 
  cycleTest1, cycleTest0, cycleTestRes0, repeatTestRes1, repeatTest1, repeatTestRes2, repeatTest2, repeatTest0, repeatTestRes0, iterateTestRes2, iterateTest2, 
  iterateTestRes0, iterateTest0, iterateTest1, appendTest3, appendTestRes3, takeTest0, takeTestRes0, takeTest1, takeTestRes1, takeTestRes2, takeTest2, takeTest3, 
  takeTestRes3, splitAtTestRes3, splitAtTest3, modTest0, modTestRes0, modTestRes1, zipWithTest2, zipWithTest0, zipWithTest1, zipWithTestRes0, modTest1, modTest2, 
  modTestRes3, modTestRes2, modTestRes4, modTest3, modTest4, zipWithTestRes2, zipWithTestRes1, divTest0, divTestRes0, divTest1, divTestRes1, divTestRes2, divTest2, 
  div2TestRes0, div2Test0, div2TestRes1, div2Test1, div2TestRes2, div2Test2, minusTest0, minusTestRes0, minusTestRes1, minusTest1, minusTestRes2, minusTest2, 
  minus2TestRes0, minus2Test0, minus2Test1, minus2TestRes1, minus2TestRes2, minus2Test2, ltTest0, ltTestRes0, ltTestRes1, ltTestRes2, ltTest2, ltTest1, leqTestRes0, 
  leqTest0, leqTest1, leqTestRes1, leqTestRes2, leqTest2, eqTest0, eqTestRes0, eqTestRes1, eqTest1, eqTestRes2, eqTest2, neqTestRes0, neqTest0, neqTestRes1, neqTest1, 
  neqTest2, neqTestRes2, geqTestRes0, geqTestRes1, geqTestRes2, geqTest2, geqTest1, geqTest0, gtTestRes0, gtTestRes1, gtTestRes2, gtTest2, gtTest0, gtTest1, evenTest0, 
  evenTestRes0, evenTestRes1, evenTest1, evenTestRes2, evenTest2, evenTestRes3, evenTest3, compTest3, compTestRes3, neqTestRes3, neqTest3, neqTestRes4, neqTest4, neqTest5, 
  neqTestRes5, neqTestRes6, neqTest6, neqTest7, neqTestRes7, eqTest3, eqTestRes3, eqTestRes4, eqTest4, eqTestRes5, eqTest5, eqTestRes6, eqTest6, eqTestRes7, eqTest7)

spec :: Spec
spec = do

  describe "Test id 0" $ do
    it "" $ do
      runCompiler idTest0 @?= idTestRes0

  describe "Test id 1" $ do
    it "" $ do
      runCompiler idTest1 @?= idTestRes1

  describe "Test id 2" $ do
    it "" $ do
      runCompiler idTest2 @?= idTestRes2


  describe "Test until 0" $ do
    it "" $ do
      runCompiler untilTest0 @?= untilTestRes0

  describe "Test until 1" $ do
    it "" $ do
      runCompiler untilTest1 @?= untilTestRes1

  describe "Test until 2" $ do
    it "" $ do
      runCompiler untilTest2 @?= untilTestRes2


  describe "Test comp 0" $ do
    it "" $ do
      runCompiler compTest0 @?= compTestRes0

  describe "Test comp 1" $ do
    it "" $ do
      runCompiler compTest1 @?= compTestRes1

  describe "Test comp 2" $ do
    it "" $ do
      runCompiler compTest2 @?= compTestRes2

  describe "Test comp 3" $ do
    it "" $ do
      runCompiler compTest3 @?= compTestRes3


  describe "Test map 0" $ do
    it "" $ do
      runCompiler mapTest0 @?= mapTestRes0

  describe "Test map 1" $ do
    it "" $ do
      runCompiler mapTest1 @?= mapTestRes1

  describe "Test map 2" $ do
    it "" $ do
      runCompiler mapTest2 @?= mapTestRes2


  describe "Test fold 0" $ do
    it "" $ do
      runCompiler foldTest0 @?= foldTestRes0

  describe "Test fold 1" $ do
    it "" $ do
      runCompiler foldTest1 @?= foldTestRes1

  describe "Test fold 2" $ do
    it "" $ do
      runCompiler foldTest2 @?= foldTestRes2


  describe "Test append 0" $ do
    it "" $ do
      runCompiler appendTest0 @?= appendTestRes0

  describe "Test append 1" $ do
    it "" $ do
      runCompiler appendTest1 @?= appendTestRes1

  describe "Test append 2" $ do
    it "" $ do
      runCompiler appendTest2 @?= appendTestRes2

  describe "Test append 3" $ do
    it "" $ do
      runCompiler appendTest3 @?= appendTestRes3


  describe "Test reverse 0" $ do
    it "" $ do
      runCompiler reverseTest0 @?= reverseTestRes0

  describe "Test reverse 1" $ do
    it "" $ do
      runCompiler reverseTest1 @?= reverseTestRes1

  describe "Test reverse 2" $ do
    it "" $ do
      runCompiler reverseTest2 @?= reverseTestRes2


  describe "Test filter 0" $ do
    it "" $ do
      runCompiler filterTest0 @?= filterTestRes0

  describe "Test filter 1" $ do
    it "" $ do
      runCompiler filterTest1 @?= filterTestRes1

  describe "Test filter 2" $ do
    it "" $ do
      runCompiler filterTest2 @?= filterTestRes2


  describe "Test sort 0" $ do
    it "" $ do
      runCompiler sortTest0 @?= sortTestRes0

  describe "Test sort 1" $ do
    it "" $ do
      runCompiler sortTest1 @?= sortTestRes1

  describe "Test sort 2" $ do
    it "" $ do
      runCompiler sortTest2 @?= sortTestRes2


  describe "Test drop 0" $ do
    it "" $ do
      runCompiler dropTest0 @?= dropTestRes0

  describe "Test drop 1" $ do
    it "" $ do
      runCompiler dropTest1 @?= dropTestRes1

  describe "Test drop 2" $ do
    it "" $ do
      runCompiler dropTest2 @?= dropTestRes2


  describe "Test take 0" $ do
    it "" $ do
      runCompiler takeTest0 @?= takeTestRes0

  describe "Test take 1" $ do
    it "" $ do
      runCompiler takeTest1 @?= takeTestRes1

  describe "Test take 2" $ do
    it "" $ do
      runCompiler takeTest2 @?= takeTestRes2

  describe "Test take 3" $ do
    it "" $ do
      runCompiler takeTest3 @?= takeTestRes3


  describe "Test at 0" $ do
    it "" $ do
      runCompiler atTest0 @?= atTestRes0

  describe "Test at 1" $ do
    it "" $ do
      runCompiler atTest1 @?= atTestRes1

  describe "Test at 2" $ do
    it "" $ do
      runCompiler atTest2 @?= atTestRes2


  describe "Test length 0" $ do
    it "" $ do
      runCompiler lengthTest0 @?= lengthTestRes0

  describe "Test length 1" $ do
    it "" $ do
      runCompiler lengthTest1 @?= lengthTestRes1

  describe "Test length 2" $ do
    it "" $ do
      runCompiler lengthTest2 @?= lengthTestRes2


  describe "Test null 0" $ do
    it "" $ do
      runCompiler nullTest0 @?= nullTestRes0

  describe "Test null 1" $ do
    it "" $ do
      runCompiler nullTest1 @?= nullTestRes1

  describe "Test null 2" $ do
    it "" $ do
      runCompiler nullTest2 @?= nullTestRes2


  describe "Test init 0" $ do
    it "" $ do
      runCompiler initTest0 @?= initTestRes0

  describe "Test init 1" $ do
    it "" $ do
      runCompiler initTest1 @?= initTestRes1

  describe "Test init 2" $ do
    it "" $ do
      runCompiler initTest2 @?= initTestRes2


  describe "Test iterate 0" $ do
    it "" $ do
      runCompiler iterateTest0 @?= iterateTestRes0

  describe "Test iterate 1" $ do
    it "" $ do
      runCompiler iterateTest1 @?= iterateTestRes1

  describe "Test iterate 2" $ do
    it "" $ do
      runCompiler iterateTest2 @?= iterateTestRes2


  describe "Test repeat 0" $ do
    it "" $ do
      runCompiler repeatTest0 @?= repeatTestRes0

  describe "Test repeat 1" $ do
    it "" $ do
      runCompiler repeatTest1 @?= repeatTestRes1

  describe "Test repeat 2" $ do
    it "" $ do
      runCompiler repeatTest2 @?= repeatTestRes2


  describe "Test cycle 0" $ do
    it "" $ do
      runCompiler cycleTest0 @?= cycleTestRes0

  describe "Test cycle 1" $ do
    it "" $ do
      runCompiler cycleTest1 @?= cycleTestRes1

  describe "Test cycle 2" $ do
    it "" $ do
      runCompiler cycleTest2 @?= cycleTestRes2


  describe "Test splitAt 0" $ do
    it "" $ do
      runCompiler splitAtTest0 @?= splitAtTestRes0

  describe "Test splitAt 1" $ do
    it "" $ do
      runCompiler splitAtTest1 @?= splitAtTestRes1

  describe "Test splitAt 2" $ do
    it "" $ do
      runCompiler splitAtTest2 @?= splitAtTestRes2

  describe "Test splitAt 3" $ do
    it "" $ do
      runCompiler splitAtTest3 @?= splitAtTestRes3


  describe "Test takeWhile 0" $ do
    it "" $ do
      runCompiler takeWhileTest0 @?= takeWhileTestRes0

  describe "Test takeWhile 1" $ do
    it "" $ do
      runCompiler takeWhileTest1 @?= takeWhileTestRes1

  describe "Test takeWhile 2" $ do
    it "" $ do
      runCompiler takeWhileTest2 @?= takeWhileTestRes2


  describe "Test sum 0" $ do
    it "" $ do
      runCompiler sumTest0 @?= sumTestRes0

  describe "Test sum 1" $ do
    it "" $ do
      runCompiler sumTest1 @?= sumTestRes1

  describe "Test sum 2" $ do
    it "" $ do
      runCompiler sumTest2 @?= sumTestRes2


  describe "Test product 0" $ do
    it "" $ do
      runCompiler productTest0 @?= productTestRes0

  describe "Test product 1" $ do
    it "" $ do
      runCompiler productTest1 @?= productTestRes1

  describe "Test product 2" $ do
    it "" $ do
      runCompiler productTest2 @?= productTestRes2
  

  describe "Test plus 0" $ do
    it "" $ do
      runCompiler plusTest0 @?= plusTestRes0

  describe "Test plus 1" $ do
    it "" $ do
      runCompiler plusTest1 @?= plusTestRes1

  describe "Test plus 2" $ do
    it "" $ do
      runCompiler plusTest2 @?= plusTestRes2


  describe "Test mul 0" $ do
    it "" $ do
      runCompiler mulTest0 @?= mulTestRes0

  describe "Test mul 1" $ do
    it "" $ do
      runCompiler mulTest1 @?= mulTestRes1

  describe "Test mul 2" $ do
    it "" $ do
      runCompiler mulTest2 @?= mulTestRes2


  describe "Test mod 0" $ do
    it "" $ do
      runCompiler modTest0 @?= modTestRes0

  describe "Test mod 1" $ do
    it "" $ do
      runCompiler modTest1 @?= modTestRes1

  describe "Test mod 2" $ do
    it "" $ do
      runCompiler modTest2 @?= modTestRes2

  describe "Test mod 3" $ do
    it "" $ do
      runCompiler modTest3 @?= modTestRes3

  describe "Test mod 4" $ do
    it "" $ do
      runCompiler modTest4 @?= modTestRes4


  describe "Test even 0" $ do
    it "" $ do
      runCompiler evenTest0 @?= evenTestRes0

  describe "Test even 1" $ do
    it "" $ do
      runCompiler evenTest1 @?= evenTestRes1

  describe "Test even 2" $ do
    it "" $ do
      runCompiler evenTest2 @?= evenTestRes2

  describe "Test even 3" $ do
    it "" $ do
      runCompiler evenTest3 @?= evenTestRes3


  describe "Test zipWith 0" $ do
    it "" $ do
      runCompiler zipWithTest0 @?= zipWithTestRes0

  describe "Test zipWith 1" $ do
    it "" $ do
      runCompiler zipWithTest1 @?= zipWithTestRes1

  describe "Test zipWith 2" $ do
    it "" $ do
      runCompiler zipWithTest2 @?= zipWithTestRes2


  describe "Test div 0" $ do
    it "" $ do
      runCompiler divTest0 @?= divTestRes0

  describe "Test div 1" $ do
    it "" $ do
      runCompiler divTest1 @?= divTestRes1

  describe "Test div 2" $ do
    it "" $ do
      runCompiler divTest2 @?= divTestRes2


  describe "Test div2 0" $ do
    it "" $ do
      runCompiler div2Test0 @?= div2TestRes0

  describe "Test div2 1" $ do
    it "" $ do
      runCompiler div2Test1 @?= div2TestRes1

  describe "Test div2 2" $ do
    it "" $ do
      runCompiler div2Test2 @?= div2TestRes2


  describe "Test minus 0" $ do
    it "" $ do
      runCompiler minusTest0 @?= minusTestRes0

  describe "Test minus 1" $ do
    it "" $ do
      runCompiler minusTest1 @?= minusTestRes1

  describe "Test minus 2" $ do
    it "" $ do
      runCompiler minusTest2 @?= minusTestRes2


  describe "Test minus2 0" $ do
    it "" $ do
      runCompiler minus2Test0 @?= minus2TestRes0

  describe "Test minus2 1" $ do
    it "" $ do
      runCompiler minus2Test1 @?= minus2TestRes1

  describe "Test minus2 2" $ do
    it "" $ do
      runCompiler minus2Test2 @?= minus2TestRes2


  describe "Test lt 0" $ do
    it "" $ do
      runCompiler ltTest0 @?= ltTestRes0

  describe "Test lt 1" $ do
    it "" $ do
      runCompiler ltTest1 @?= ltTestRes1

  describe "Test lt 2" $ do
    it "" $ do
      runCompiler ltTest2 @?= ltTestRes2


  describe "Test leq 0" $ do
    it "" $ do
      runCompiler leqTest0 @?= leqTestRes0

  describe "Test leq 1" $ do
    it "" $ do
      runCompiler leqTest1 @?= leqTestRes1

  describe "Test leq 2" $ do
    it "" $ do
      runCompiler leqTest2 @?= leqTestRes2


  describe "Test eq 0" $ do
    it "" $ do
      runCompiler eqTest0 @?= eqTestRes0

  describe "Test eq 1" $ do
    it "" $ do
      runCompiler eqTest1 @?= eqTestRes1

  describe "Test eq 2" $ do
    it "" $ do
      runCompiler eqTest2 @?= eqTestRes2

  describe "Test eq 3" $ do
    it "" $ do
      runCompiler eqTest3 @?= eqTestRes3

  describe "Test eq 4" $ do
    it "" $ do
      runCompiler eqTest4 @?= eqTestRes4

  describe "Test eq 5" $ do
    it "" $ do
      runCompiler eqTest5 @?= eqTestRes5

  describe "Test eq 6" $ do
    it "" $ do
      runCompiler eqTest6 @?= eqTestRes6

  describe "Test eq 7" $ do
    it "" $ do
      runCompiler eqTest7 @?= eqTestRes7


  describe "Test neq 0" $ do
    it "" $ do
      runCompiler neqTest0 @?= neqTestRes0

  describe "Test neq 1" $ do
    it "" $ do
      runCompiler neqTest1 @?= neqTestRes1

  describe "Test neq 2" $ do
    it "" $ do
      runCompiler neqTest2 @?= neqTestRes2

  describe "Test neq 3" $ do
    it "" $ do
      runCompiler neqTest3 @?= neqTestRes3

  describe "Test neq 4" $ do
    it "" $ do
      runCompiler neqTest4 @?= neqTestRes4

  describe "Test neq 5" $ do
    it "" $ do
      runCompiler neqTest5 @?= neqTestRes5

  describe "Test neq 6" $ do
    it "" $ do
      runCompiler neqTest6 @?= neqTestRes6

  describe "Test neq 7" $ do
    it "" $ do
      runCompiler neqTest7 @?= neqTestRes7


  describe "Test geq 0" $ do
    it "" $ do
      runCompiler geqTest0 @?= geqTestRes0

  describe "Test geq 1" $ do
    it "" $ do
      runCompiler geqTest1 @?= geqTestRes1

  describe "Test geq 2" $ do
    it "" $ do
      runCompiler geqTest2 @?= geqTestRes2


  describe "Test gt 0" $ do
    it "" $ do
      runCompiler gtTest0 @?= gtTestRes0

  describe "Test gt 1" $ do
    it "" $ do
      runCompiler gtTest1 @?= gtTestRes1

  describe "Test gt 2" $ do
    it "" $ do
      runCompiler gtTest2 @?= gtTestRes2