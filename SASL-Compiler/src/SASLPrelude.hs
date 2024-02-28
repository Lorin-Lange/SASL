{-|
Module      : SASLPrelude
Description : SASLPrelude
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains common functions of a prelude
as SASL-programs (Strings). There're used for the REPL,
unit testing and benchmarking.
-}

module SASLPrelude where

import Prelude hiding
   (even, div, zipWith, mod, product, sum, takeWhile, 
   splitAt, cycle, repeat,
   iterate, init, null, length, take, drop, reverse,
   filter, id, until, map)

idTest0 :: String
idTest0 = " . id 5 "

idTestRes0 :: String
idTestRes0 = "5"

idTest1 :: String
idTest1 = " . (id \\a . a + 1) 1 "

idTestRes1 :: String
idTestRes1 = "2"

idTest2 :: String
idTest2 = " . id (id 5) "

idTestRes2 :: String
idTestRes2 = "5"

untilTest0 :: String
untilTest0 = " . until (\\ a . a > 100) (\\ a . a * 2) 1"

untilTestRes0 :: String
untilTestRes0 = "128"

untilTest1 :: String
untilTest1 = " . until (\\ i . not (even i)) (\\ i . i / 2) 400"

untilTestRes1 :: String
untilTestRes1 = "25"

untilTest2 :: String
untilTest2 = " . until (\\i . even i) (\\i . i + 1 ) 1"

untilTestRes2 :: String
untilTestRes2 = "2"

compTest0 :: String
compTest0 = " . comp (\\i . 4 + i) (\\ i . 3 * i) 5"

compTestRes0 :: String
compTestRes0 = "19"

compTest1 :: String
compTest1 = " . comp (\\i . 4 * i) (\\ i . 3 + i) 5"

compTestRes1 :: String
compTestRes1 = "32"

compTest2 :: String
compTest2 = " . comp (\\i . i + 1) id 1"

compTestRes2 :: String
compTestRes2 = "2"

compTest3 :: String
compTest3 = " . comp id (\\i . i + 1) 1"

compTestRes3 :: String
compTestRes3 = "2"

mapTest0 :: String
mapTest0 = " . map (\\a . a * a) [1,2,3]"

mapTestRes0 :: String
mapTestRes0 = "[1,4,9]"

mapTest1 :: String
mapTest1 = " . map (\\a . a * a) []"

mapTestRes1 :: String
mapTestRes1 = "[]"

mapTest2 :: String
mapTest2 = " . map id [1,2,3]"

mapTestRes2 :: String
mapTestRes2 = "[1,2,3]"

foldTest0 :: String
foldTest0 = " . fold append [] [[1],[2],[3]]"

foldTestRes0 :: String
foldTestRes0 = "[1,2,3]"

foldTest1 :: String
foldTest1 = " . fold (\\ l r . l + r) 1 [2,3,4]"

foldTestRes1 :: String
foldTestRes1 = "10"

foldTest2 :: String
foldTest2 = " . fold (\\ l r . l + r) 1 []"

foldTestRes2 :: String
foldTestRes2 = "1"

appendTest0 :: String
appendTest0 = " . append [] []"

appendTestRes0 :: String
appendTestRes0 = "[]"

appendTest1 :: String
appendTest1 = " . append [] [1,2,3]"

appendTestRes1 :: String
appendTestRes1 = "[1,2,3]"

appendTest2 :: String
appendTest2 = " . append [1,2,3] []"

appendTestRes2 :: String
appendTestRes2 = "[1,2,3]"

appendTest3 :: String
appendTest3 = " . append [1,2,3] [4,5,6]"

appendTestRes3 :: String
appendTestRes3 = "[1,2,3,4,5,6]"

reverseTest0 :: String
reverseTest0 = " . reverse []"

reverseTestRes0 :: String
reverseTestRes0 = "[]"

reverseTest1 :: String
reverseTest1 = " . reverse [1,2,true]"

reverseTestRes1 :: String
reverseTestRes1 = "[true,2,1]"

reverseTest2 :: String
reverseTest2 = " . reverse (reverse [1,2,true])"

reverseTestRes2 :: String
reverseTestRes2 = "[1,2,true]"

filterTest0 :: String
filterTest0 = " . filter (\\ a . a > 5) [1,2,3,4,5,6,7,8]"

filterTestRes0 :: String
filterTestRes0 = "[6,7,8]"

filterTest1 :: String
filterTest1 = " . filter (\\ i . neq (mod i 2) 0) [3,6,7,9,12,14]"

filterTestRes1 :: String
filterTestRes1 = "[3,7,9]"

filterTest2 :: String
filterTest2 = " . filter (\\x . length x > 3) [[], [1], [1,2], [1,2,3], [1,2,3,4]]"

filterTestRes2 :: String
filterTestRes2 = "[[1,2,3,4]]"

sortTest0 :: String
sortTest0 = " . sort (\\ a b . a < b) []"

sortTestRes0 :: String
sortTestRes0 = "[]"

sortTest1 :: String
sortTest1 = " . sort (\\ a b . a < b) [3348,-4513,1457,-3128,2682,3348,-580,376,-186,-608,2665]" 

sortTestRes1 :: String
sortTestRes1 = "[-4513,-3128,-608,-580,-186,376,1457,2665,2682,3348,3348]"

sortTest2 :: String
sortTest2 = " . sort (\\a b . (length a) > (length b)) [[5,6,89,2], [], [1,2,3], [true, 2], [1]]"

sortTestRes2 :: String
sortTestRes2 = "[[5,6,89,2],[1,2,3],[true,2],[1],[]]"

dropTest0 :: String
dropTest0 = " . drop 1 [1,2,3]"

dropTestRes0 :: String
dropTestRes0 = "[2,3]"

dropTest1 :: String
dropTest1 = " . drop (-1) [1,2,3]"

dropTestRes1 :: String
dropTestRes1 = "[1,2,3]"

dropTest2 :: String
dropTest2 = " . drop 100 []"

dropTestRes2 :: String
dropTestRes2 = "[]"

takeTest0 :: String
takeTest0 = " . take (-5) [1,2,3,4,5]"

takeTestRes0 :: String
takeTestRes0 = "[]"

takeTest1 :: String
takeTest1 = " . take 1 []"

takeTestRes1 :: String
takeTestRes1 = "[]"

takeTest2 :: String
takeTest2 = " . take 10 [1,2,3,4,5]"

takeTestRes2 :: String
takeTestRes2 = "[1,2,3,4,5]"

takeTest3 :: String
takeTest3 = " . take 3 [1,2,3,4,5]"

takeTestRes3 :: String
takeTestRes3 = "[1,2,3]"

atTest0 :: String
atTest0 = " . at 2 [1,2,3,4,5]"

atTestRes0 :: String
atTestRes0 = "3"

atTest1 :: String
atTest1 = " . at 0 [true]"

atTestRes1 :: String
atTestRes1 = "true"

atTest2 :: String
atTest2 = " . at 1 [true, \"Hello World\", 42]"

atTestRes2 :: String
atTestRes2 = "\"Hello World\""

lengthTest0 :: String
lengthTest0 = " . length [(\\a . dfjwidfj), (\\a . sdfjslfkj), (\\a . sldfjsdjf)]"

lengthTestRes0 :: String
lengthTestRes0 = "3"

lengthTest1 :: String
lengthTest1 = " . length [dfjwidfj, sdfjslfkj, sldfjsdjf]"

lengthTestRes1 :: String
lengthTestRes1 = "3"

lengthTest2 :: String
lengthTest2 = " . length []"

lengthTestRes2 :: String
lengthTestRes2 = "0"

nullTest0 :: String
nullTest0 = " . null []"

nullTestRes0 :: String
nullTestRes0 = "true"

nullTest1 :: String
nullTest1 = " . null [1,2,3]"

nullTestRes1 :: String
nullTestRes1 = "false"

nullTest2 :: String
nullTest2 = " . null lst where lst = 1:lst"

nullTestRes2 :: String
nullTestRes2 = "false"

initTest0 :: String
initTest0 = " . init [1,2,3] "

initTestRes0 :: String
initTestRes0 = "[1,2]"

initTest1 :: String
initTest1 = " . init [1,2] "

initTestRes1 :: String
initTestRes1 = "[1]"

initTest2 :: String
initTest2 = " . init (init [1,2,3] ) "

initTestRes2 :: String
initTestRes2 = "[1]"

iterateTest0 :: String
iterateTest0 = " . take 10 (iterate (\\i . 2 * i) 1)"

iterateTestRes0 :: String
iterateTestRes0 = "[1,2,4,8,16,32,64,128,256,512]"

iterateTest1 :: String
iterateTest1 = " . take 10 (iterate (\\x . (x+3)*2) 1)"

iterateTestRes1 :: String
iterateTestRes1 = "[1,8,22,50,106,218,442,890,1786,3578]"

iterateTest2 :: String
iterateTest2 = " . take 10 (iterate (\\i . not i) true)"

iterateTestRes2 :: String
iterateTestRes2 = "[true,false,true,false,true,false,true,false,true,false]"

repeatTest0 :: String
repeatTest0 = " . take 4 (repeat 3) "

repeatTestRes0 :: String
repeatTestRes0 = "[3,3,3,3]"

repeatTest1 :: String
repeatTest1 = " . take 3 (repeat \"r\")"

repeatTestRes1 :: String
repeatTestRes1 = "[\"r\",\"r\",\"r\"]"

repeatTest2 :: String
repeatTest2 = " . take 7 (repeat true)"

repeatTestRes2 :: String
repeatTestRes2 = "[true,true,true,true,true,true,true]"

cycleTest0 :: String
cycleTest0 = " . take 10 (cycle [1,2,3])"

cycleTestRes0 :: String
cycleTestRes0 = "[1,2,3,1,2,3,1,2,3,1]"

cycleTest1 :: String
cycleTest1 = " . take 3 (cycle [42])"

cycleTestRes1 :: String
cycleTestRes1 = "[42,42,42]"

cycleTest2 :: String
cycleTest2 = " . take 6 (cycle [true, false])"

cycleTestRes2 :: String
cycleTestRes2 = "[true,false,true,false,true,false]"

splitAtTest0 :: String
splitAtTest0 = " . splitAt 0 [1,2,3,4,5]"

splitAtTestRes0 :: String
splitAtTestRes0 = "[[],1,2,3,4,5]"

splitAtTest1 :: String
splitAtTest1 = " . splitAt 2 [1,2,3,4,5]"

splitAtTestRes1 :: String
splitAtTestRes1 = "[[1,2],3,4,5]"

splitAtTest2 :: String
splitAtTest2 = " . splitAt 1 []"

splitAtTestRes2 :: String
splitAtTestRes2 = "[[]]"

splitAtTest3 :: String
splitAtTest3 = " . splitAt 0 []"

splitAtTestRes3 :: String
splitAtTestRes3 = "[[]]"

takeWhileTest0 :: String
takeWhileTest0 = " . takeWhile (\\i . i < 4) [1,2,3,4,5]"

takeWhileTestRes0 :: String
takeWhileTestRes0 = "[1,2,3]"

takeWhileTest1 :: String
takeWhileTest1 = " . takeWhile (\\i . i > 4) [1,2,3,4,5]"

takeWhileTestRes1 :: String
takeWhileTestRes1 = "[]"

takeWhileTest2 :: String
takeWhileTest2 = " . takeWhile (\\i . i > 4) []"

takeWhileTestRes2 :: String
takeWhileTestRes2 = "[]"

sumTest0 :: String
sumTest0 = " . sum [1,2,3,4]"

sumTestRes0 :: String
sumTestRes0 = "10"

sumTest1 :: String
sumTest1 = " . sum []"

sumTestRes1 :: String
sumTestRes1 = "0"

sumTest2 :: String
sumTest2 = " . sum [-1,2,-3,-4]"

sumTestRes2 :: String
sumTestRes2 = "-6"

productTest0 :: String
productTest0 = " . product [1,2,3,4]"

productTestRes0 :: String
productTestRes0 = "24"

productTest1 :: String
productTest1 = " . product []"

productTestRes1 :: String
productTestRes1 = "1"

productTest2 :: String
productTest2 = " . product [-1,-2,-3]"

productTestRes2 :: String
productTestRes2 = "-6"

plusTest0 :: String
plusTest0 = " . plus (-(-1)) 2"

plusTestRes0 :: String
plusTestRes0 = "3"

plusTest1 :: String
plusTest1 =  " . plus 1 (-2)"

plusTestRes1 :: String
plusTestRes1 = "-1"

plusTest2 :: String
plusTest2 =  " . plus (-1) (-2)"

plusTestRes2 :: String
plusTestRes2 = "-3"

mulTest0 :: String
mulTest0 = " . mul (+12) (+(-13))"

mulTestRes0 :: String
mulTestRes0 = "-156"

mulTest1 :: String
mulTest1 = " . mul (-13) (-13)"

mulTestRes1 :: String
mulTestRes1 = "169"

mulTest2 :: String
mulTest2 = " . mul (-0) (-13)"

mulTestRes2 :: String
mulTestRes2 = "0"

modTest0 :: String
modTest0 = " . mod 7 5"

modTestRes0 :: String
modTestRes0 = "2"

modTest1 :: String
modTest1 = " . mod 70 7"

modTestRes1 :: String
modTestRes1 = "0"

modTest2 :: String
modTest2 = " . mod 156 (-10)"

modTestRes2 :: String
modTestRes2 = "-4"

modTest3 :: String
modTest3 = " . mod (-100) (-10)"

modTestRes3 :: String
modTestRes3 = "0"

modTest4 :: String
modTest4 = " . mod 4534 1"

modTestRes4 :: String
modTestRes4 = "0"

evenTest0 :: String
evenTest0 = " . even 12"

evenTestRes0 :: String
evenTestRes0 = "true"

evenTest1 :: String
evenTest1 = " . even 13"

evenTestRes1 :: String
evenTestRes1 = "false"

evenTest2 :: String
evenTest2 = " . even 0"

evenTestRes2 :: String
evenTestRes2 = "true"

evenTest3 :: String
evenTest3 = " . even (-12)"

evenTestRes3 :: String
evenTestRes3 = "true"

zipWithTest0 :: String
zipWithTest0 = " . take 10 fibs where fibs = 0 : 1 : zipWith plus fibs (tl fibs)"

zipWithTestRes0 :: String
zipWithTestRes0 = "[0,1,1,2,3,5,8,13,21,34]"

zipWithTest1 :: String
zipWithTest1 = " . zipWith (\\a b . a + b + 1 ) [0,1,2] [3,4,5,6,7]"

zipWithTestRes1 :: String
zipWithTestRes1 = "[4,6,8]"

zipWithTest2 :: String
zipWithTest2 = " . zipWith (\\a b . a and b) [true, false, true, false] [true, true, true]"

zipWithTestRes2 :: String
zipWithTestRes2 = "[true,false,true]"

divTest0 :: String
divTest0 = " . div 4 (-2)"

divTestRes0 :: String
divTestRes0 = "-2"

divTest1 :: String
divTest1 = " . div (-4) (-2)"

divTestRes1 :: String
divTestRes1 = "2"

divTest2 :: String
divTest2 = " . div 4 3"

divTestRes2 :: String
divTestRes2 = "1"

div2Test0 :: String
div2Test0 = " . div2 4 (-2)"

div2TestRes0 :: String
div2TestRes0 = "-1"

div2Test1 :: String
div2Test1 = " . div2 (-20) (-120)"

div2TestRes1 :: String
div2TestRes1 = "6"

div2Test2 :: String
div2Test2 = " . div2 4 8"

div2TestRes2 :: String
div2TestRes2 = "2"

minusTest0 :: String
minusTest0 = " . minus (-5) 5 "

minusTestRes0 :: String
minusTestRes0 = "-10"

minusTest1 :: String
minusTest1 = " . minus (-5) (-5) "

minusTestRes1 :: String
minusTestRes1 = "0"

minusTest2 :: String
minusTest2 = " . minus (+5) (+7) "

minusTestRes2 :: String
minusTestRes2 = "-2"

minus2Test0 :: String
minus2Test0 = " . minus2 (-5) 5 "

minus2TestRes0 :: String
minus2TestRes0 = "10"

minus2Test1 :: String
minus2Test1 = " . minus2 (-5) (-5) "

minus2TestRes1 :: String
minus2TestRes1 = "0"

minus2Test2 :: String
minus2Test2 = " . minus2 (+5) (+7) "

minus2TestRes2 :: String
minus2TestRes2 = "2"

ltTest0 :: String
ltTest0 = " . lt 4 5"

ltTestRes0 :: String
ltTestRes0 = "true"

ltTest1 :: String
ltTest1 = " . lt 4 4"

ltTestRes1 :: String
ltTestRes1 = "false"

ltTest2 :: String
ltTest2 = " . lt 5 4"

ltTestRes2 :: String
ltTestRes2 = "false"

leqTest0 :: String
leqTest0 = " . leq 4 5"

leqTestRes0 :: String
leqTestRes0 = "true"

leqTest1 :: String
leqTest1 = " . leq 4 4"

leqTestRes1 :: String
leqTestRes1 = "true"

leqTest2 :: String
leqTest2 = " . leq 5 4"

leqTestRes2 :: String
leqTestRes2 = "false"

eqTest0 :: String
eqTest0 = " . eq [0,1,2] [0,true,2]"

eqTestRes0 :: String
eqTestRes0 = "false"

eqTest1 :: String
eqTest1 = " . eq [1,2,3] [1,2,3]"

eqTestRes1 :: String
eqTestRes1 = "true"

eqTest2 :: String
eqTest2 = " . eq (-1) (-1)"

eqTestRes2 :: String
eqTestRes2 = "true"

eqTest3 :: String
eqTest3 = " . eq (-1) 1"

eqTestRes3 :: String
eqTestRes3 = "false"

eqTest4 :: String
eqTest4 = " . eq \"abc\" \"abc\""

eqTestRes4 :: String
eqTestRes4 = "true"

eqTest5 :: String
eqTest5 = " . eq \"abc \" \"abc\""

eqTestRes5 :: String
eqTestRes5 = "false"

eqTest6 :: String
eqTest6 = " . eq false false"

eqTestRes6 :: String
eqTestRes6 = "true"

eqTest7 :: String
eqTest7 = " . eq true false"

eqTestRes7 :: String
eqTestRes7 = "false"

neqTest0 :: String
neqTest0 = " . neq [1,2,3] [1,2,3]"

neqTestRes0 :: String
neqTestRes0 = "false"

neqTest1 :: String
neqTest1 = " . neq [0,1,2] [0,true,2] "

neqTestRes1 :: String
neqTestRes1 = "true"

neqTest2 :: String
neqTest2 = " . neq (-1) (-1)"

neqTestRes2 :: String
neqTestRes2 = "false"

neqTest3 :: String
neqTest3 = " . neq (-1) 1"

neqTestRes3 :: String
neqTestRes3 = "true"

neqTest4 :: String
neqTest4 = " . neq \"abc\" \"abc\""

neqTestRes4 :: String
neqTestRes4 = "false"

neqTest5 :: String
neqTest5 = " . neq false false"

neqTestRes5 :: String
neqTestRes5 = "false"

neqTest6 :: String
neqTest6 = " . neq true false"

neqTestRes6 :: String
neqTestRes6 = "true"

neqTest7 :: String
neqTest7 = " . neq \"abc \" \"abc\""

neqTestRes7 :: String
neqTestRes7 = "true"

geqTest0 :: String
geqTest0 = " . geq 1 1"

geqTestRes0 :: String
geqTestRes0 = "true"

geqTest1 :: String
geqTest1 = " . geq (-50) (-70)"

geqTestRes1 :: String
geqTestRes1 = "true"

geqTest2 :: String
geqTest2 = " . geq (-70) (-50)"

geqTestRes2 :: String
geqTestRes2 = "false"

gtTest0 :: String
gtTest0 = " . gt 1 1"

gtTestRes0 :: String
gtTestRes0 = "false"

gtTest1 :: String
gtTest1 = " . gt 2 1"

gtTestRes1 :: String
gtTestRes1 = "true"

gtTest2 :: String
gtTest2 = " . gt (-1) (-2)"

gtTestRes2 :: String
gtTestRes2 = "true"

id :: String
id = "def id x = x "

until :: String
until = "def until p f x = if p x then x else until p f (f x) "

comp :: String
comp = "def comp f g x = f (g x) "

map :: String
map = "def map f l = if l=nil then nil                  \
      \              else f x:map f xs where x  = hd l; \
      \                                      xs = tl l  "

fold :: String
fold = "def fold m z l = if l=nil then z                          \
       \                  else m x (fold m z xs) where x  = hd l; \
       \                                                xs = tl l "

append :: String
append = "def append l1 l2 = if l1=nil then l2                     \
         \                   else x:append xs l2 where x  = hd l1; \
         \                                              xs = tl l1 "

reverse :: String
reverse = "def reverse l = if l=nil then nil                   \
          \                else append (reverse (tl l)) [hd l] "

filter :: String
filter = "def filter p l = if l=nil then nil                              \
         \                 else (if p x then x:filter p xs                \
         \                             else filter p xs) where x  = hd l; \
         \                                                    xs = tl l   "

sort :: String
sort = "def sort p l = if l=nil then nil                                    \
       \             else insert p (hd l) (sort p (tl l))                   \
       \        where                                                       \
       \             insert pp e ll = if ll=nil then [e]                    \
       \                              else                                  \
       \                                   if pp e (hd ll) then e:ll        \
       \                                   else                             \
       \                                        (hd ll):insert pp e (tl ll) "

drop :: String
drop = "def drop n l = if n<=0 then l              \
       \               else if l=nil then nil      \
       \                    else drop (n-1) (tl l) "

take :: String
take = "def take n l = if n<=0 or l=nil then nil                       \
       \                       else (x:take (n-1) xs) where x  = hd l; \
       \                                                    xs = tl l  "

at :: String
at = "def at n l = if n=0 then hd l            \
     \                    else at (n-1) (tl l) "

length :: String
length = "def length l = if l=nil then 0               \
    \                        else 1+length (tl l) "

null :: String
null = "def null l = l = nil "

init :: String
init = "def init l = (if xs=nil then nil               \
  \              else x:init xs) where x  = hd l; \
  \                                    xs = tl l  "

iterate :: String
iterate = "def iterate f x = x : iterate f (f x) "

repeat :: String
repeat = "def repeat x = xs where xs=x:xs "

cycle :: String
cycle = "def cycle xs = xs1 where xs1=append xs xs1 "

splitAt :: String
splitAt = "def splitAt n l = if n<=0 then []:l                      \
          \                  else if l=nil then []:[]               \
          \                       else ((hd l):xs1):xs2             \
          \                          where                          \
          \                             xs  = splitAt (n-1) (tl l); \
          \                             xs1 = hd xs;                \
          \                             xs2 = tl xs                 "

takeWhile :: String
takeWhile = "def takeWhile p l = if l=nil then nil                  \
       \                    else (if p x then x:takeWhile p xs \
       \                          else nil)                    \
       \                             where                     \
       \                               x  = hd l;              \
       \                               xs = tl l               "

sum :: String
sum = "def sum = fold (\\ a b . a + b) 0                  \
      \           where                                   \
      \               fold m z l = if l=nil then z        \
      \                            else m x (fold m z xs) \
      \                                   where x  = hd l;\
      \                                         xs = tl l "

product :: String
product = "def product = fold (\\ a b . a * b) 1              \
          \              where                                \
          \               fold m z l = if l=nil then z        \
          \                            else m x (fold m z xs) \
          \                                   where x  = hd l;\
          \                                         xs = tl l "


plus :: String
plus = "def plus x y = x+y "

mul :: String
mul = "def mul x y = x*y "

mod :: String
mod = "def mod num divisor = num - divisor * (num / divisor) "

even :: String
even = "def even n = (mod n 2) = 0                                       \
       \              where                                              \
       \               mod num divisor = num - divisor * (num / divisor) "

zipWith :: String
zipWith = "def zipWith f l1 l2 = if l1 = nil or l2 = nil              \
          \                      then nil                             \
          \                      else (f x y) : zipWith f xs ys       \
          \                                          where            \
          \                                             x  = hd l1;   \
          \                                             y  = hd l2;   \
          \                                             xs = tail l1; \
          \                                             ys = tail l2; \
          \                    tail l = if l = nil then nil else tl l "

div :: String
div = "def div x y = x/y "

div2 :: String
div2 = "def div2 y x = x/y "

minus :: String
minus = "def minus x y = x-y "

minus2 :: String
minus2 = "def minus2 y x = x-y "

lt :: String
lt = "def lt x y = x<y "

leq :: String
leq = "def leq x y = x<=y "

eq :: String
eq = "def eq x y = x=y "

neq :: String
neq = "def neq x y = x~=y "

geq :: String
geq = "def geq x y = x>=y "

gt :: String
gt = "def gt x y = x>y "

prelude :: String
prelude = concat 
     [id, until, comp, map, fold, append, reverse, 
      filter, sort, drop, take, at, length, null, init, 
      iterate, repeat, cycle, splitAt, takeWhile, sum, product,
      plus, mul, mod, even, zipWith, div, div2, minus, 
      minus2, lt, leq, eq, neq, geq, gt]