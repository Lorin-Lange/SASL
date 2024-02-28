{-|
Module      : Main
Description : Benchmarking
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental
This module contains programs to benchmark
using Criterion.
-}

module Main where

import Criterion.Main ( defaultMain, bench, bgroup, whnf, nf )
import Compiler (runCompiler, runCompilerWithArgs, Args (Args))
import SASLPrelude (sortTest2, sortTest1)

str :: String
str = "def mc91 n = if n > 100 then n - 10 else mc91 (mc91 (n + 11)) . mc91 42"

str1 :: String
str1 = "def mc91 n = if n > 100 then n - 10 else mc91 (mc91 (n + 11)) . map mc91 [-100,-99,-98,-97,-96,-95,-94,-93,-92,-91,-90,-89,-88,-87,-86,-85,-84,-83,-82,-81,-80,-79,-78,-77,-76,-75,-74,-73,-72,-71,-70,-69,-68,-67,-66,-65,-64,-63,-62,-61,-60,-59,-58,-57,-56,-55,-54,-53,-52,-51,-50,-49,-48,-47,-46,-45,-44,-43,-42,-41,-40,-39,-38,-37,-36,-35,-34,-33,-32,-31,-30,-29,-28,-27,-26,-25,-24,-23,-22,-21,-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150]"

-- | <https://en.wikipedia.org/wiki/Tak_(function)>
tak :: String
tak = "def tak x y z = if x <= y then z else \
      \            tak(tak (x-1) y z)        \
      \               (tak (y-1) z x)        \
      \               (tak (z-1) x y)        \
      \ .                                    \
      \ tak 18 12 6                          "

tarai :: String
tarai = "def tarai x y z = if x <= y then y else \
        \          tarai(tarai (x-1) y z)        \
        \               (tarai (y-1) z x)        \
        \               (tarai (z-1) x y)        \
        \ .                                      \
        \ tarai 18 12 6                          "

qsort :: String
qsort = "def testList = [7465, 95339, 79651, -68109, -73630, -40957, 7465, -18042, 27844, -52486] \
        \def append3 l1 l2 = append $ append l1 l2                                                \
        \def qsort list = if list = nil then nil                                                  \
        \                               else append3 (qsort l) [p] (qsort r)                      \
        \                                 where                                                   \
        \                                  p  = hd list;                                          \
        \                                  rl = tl list;                                          \
        \                                  l  = filter (geq p) rl;                                \
        \                                  r  = filter (lt p) rl                                  \
        \ .                                                                                       \
        \ qsort testList                                                                          "

fibs :: String
fibs = "def fibs = 0 : 1 : zipWith (\\x y.x + y) fibs (tl fibs) \
       \ .                                                      \
       \ take 1000 fibs                                         "

main :: IO ()
main = defaultMain [
  bgroup "mc91" [ bench "str1"  $ nf runCompiler str1
                , bench "str"  $ nf runCompiler str
                ]
                ,
  bgroup "sort" [ bench "sortTest2" $ nf runCompiler sortTest2
                , bench "sortTest1" $ nf runCompiler sortTest1
                ]
                ,
  bgroup "tak vs. tarai" 
                [ bench "tak" $ whnf runCompiler tak
                , bench "tarai" $ whnf runCompiler tarai
                ]
                ,
  bgroup "qsort" 
                [ bench "compiled without optimizer and optimizedCompiler" $
                  whnf (runCompilerWithArgs (Args "" False False False False False)) qsort
                , bench "compiled with optimizer" $
                  whnf (runCompilerWithArgs (Args "" False True  False False False)) qsort
                , bench "compiled with optimizedCompiler" $ 
                  whnf (runCompilerWithArgs (Args "" False False True  False False)) qsort
                , bench "compiled with optimizer and optimizedCompiler" $ 
                  whnf (runCompilerWithArgs (Args "" False True  True  False False)) qsort
                ]
                ,
  bgroup "fibs" 
                [ bench "compiled without optimizer and optimizedCompiler" $
                  whnf (runCompilerWithArgs (Args "" False False False False False)) fibs
                , bench "compiled with optimizer" $
                  whnf (runCompilerWithArgs (Args "" False True  False False False)) fibs
                , bench "compiled with optimizedCompiler" $ 
                  whnf (runCompilerWithArgs (Args "" False False True  False False)) fibs
                , bench "compiled with optimizer and optimizedCompiler" $ 
                  whnf (runCompilerWithArgs (Args "" False True  True  False False)) fibs
                ]
  ]