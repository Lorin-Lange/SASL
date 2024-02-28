{-|
Module      : Frontend.Parser
Description : Parser
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO() : Longer description
-}

{-# LANGUAGE PatternSynonyms #-}

module Frontend.Parser where

import Control.Monad.State ( MonadTrans(lift) )

import Text.Megaparsec
    ( runParserT,
      errorBundlePretty,
      choice,
      many,
      some,
      eof,
      MonadParsec(try) )

import Utilities.State ( insertNode, St, setParserGraph )

import Utilities.Types
    ( SASL((:@:), Def),
      Pointer,
      nullPtr,
      pattern BNot,
      pattern BMinus,
      pattern BPlus,
      pattern BGq,
      pattern BGt,
      pattern BLq,
      pattern BLt,
      pattern BNq,
      pattern BEq,
      pattern BOr,
      pattern BAnd,
      pattern BDiv,
      pattern BMul,
      pattern BSub,
      pattern BAdd,
      pattern BNil,
      pattern BCons,
      pattern BTl,
      pattern BHd,
      pattern BCond,
      pattern BString,
      pattern BBool,
      pattern BInt,
      pattern BRef,
      pattern BB )

import Frontend.Primitives
    ( Parser,
      junk,
      word,
      keyword,
      natural,
      boolean,
      string,
      identifier )

-- * General Parsing

-- | The entire program.
program :: Parser Pointer
program = do _ <- junk
             p <- system
             _ <- junk
             _ <- eof
             return p

-- | Parsed global program structure: @global definitions . expression@
system :: Parser Pointer
system = choice $ map try
  [ do ds <- funcdefs
       _  <- word "."
       e  <- expr
       lift $ insertNode $ Def ds e True
  , expr
  ]

-- | Global definitions.
funcdefs :: Parser [([String], Pointer)]
funcdefs = some $ keyword "def" >> def

-- | Local definitions.
defs :: Parser [([String] , Pointer)]
defs = do d  <- def
          ds <- many (word ";" >> def)
          return $ d : ds

-- | A single definiton, of the form @definiendum = definiens@.
def :: Parser ([String] , Pointer)
def = do idfrs <- some identifier
         _ <- word "="
         e <- expr
         return (idfrs, e)

-- | Expression of any kind.
expr :: Parser Pointer
expr = do e <- condexpr
          choice $ map try
            [ do _  <- keyword "where"
                 ds <- defs
                 lift $ insertNode $ Def ds e False
            , return e
            ]

-- | Conditional expression.
condexpr :: Parser Pointer
condexpr = choice $ map try
  [ do _  <- keyword "if"
       e1 <- expr
       _  <- keyword "then"
       e2 <- condexpr
       _  <- keyword "else"
       e3 <- condexpr
       cd <- lift $ insertNode BCond
       e4 <- lift $ insertNode $ cd :@: e1
       e5 <- lift $ insertNode $ e4 :@: e2
       lift $ insertNode $ e5 :@: e3
  , application
  ]

-- | Right-associative function application (low precedence, operator: @$@).
application :: Parser Pointer
application = do e1 <- listexpr
                 choice $ map try
                   [ do _  <- word "$"
                        e2 <- application 
                        lift $ insertNode $ e1 :@: e2
                   , return e1
                   ]

-- | List of the form @head : tail@.
listexpr :: Parser Pointer
listexpr = do e1 <- opexpr
              choice $ map try
                [ do _  <- word ":"
                     e2 <- listexpr
                     cs <- lift $ insertNode BCons
                     e3 <- lift $ insertNode $ cs :@: e1
                     lift $ insertNode $ e3 :@: e2
                , return e1
                ]


-- | Any other infix-operator expression.
opexpr :: Parser Pointer
opexpr = do e1 <- conjunct
            choice $ map try
              [ do _  <- keyword "or"
                   e2 <- opexpr
                   op <- lift $ insertNode BOr
                   e3 <- lift $ insertNode $ op :@: e1
                   lift $ insertNode $ e3 :@: e2
              , return e1
              ]

-- | Logical conjunction.
conjunct :: Parser Pointer
conjunct = do e1 <- compar
              choice $ map try
                [ do _  <- keyword "and"
                     e2 <- conjunct
                     op <- lift $ insertNode BAnd
                     e3 <- lift $ insertNode $ op :@: e1
                     lift $ insertNode $ e3 :@: e2
                , return e1
                ]

-- | Numeric comparisons.
compar :: Parser Pointer
compar = do e1 <- add
            choice $ map try
              [ do op <- relop
                   e2 <- compar
                   e3 <- lift $ insertNode $ op :@: e1
                   lift $ insertNode $ e3 :@: e2
              , return e1
              ]

-- | Addition and Substraction.
add :: Parser Pointer
add = do e1 <- mul
         choice $ map try
           [ do op <- addop
                e2 <- add
                e3 <- lift $ insertNode $ op :@: e1
                lift $ insertNode $ e3 :@: e2
           , return e1
           ]

-- | Multiplication and Division.
mul :: Parser Pointer
mul = do e1 <- factor
         choice $ map try
           [ do op <- mulop
                e2 <- mul
                e3 <- lift $ insertNode $ op :@: e1
                lift $ insertNode $ e3 :@: e2
           , return e1
           ]

-- | Unary-operator expressions.
factor :: Parser Pointer
factor = choice $ map try
  [ do op <- prefix
       e  <- comb
       lift $ insertNode $ op :@: e
  , composition
  ]

-- | Function composition, operator: @'@
composition :: Parser Pointer
composition = do e1 <- comb
                 choice $ map try
                   [ do _  <- word "'"
                        e2 <- composition
                        b  <- lift $ insertNode BB
                        e3 <- lift $ insertNode $ b :@: e1
                        lift $ insertNode $ e3 :@: e2
                   , return e1
                   ]

-- | Function application (juxtaposition).
comb :: Parser Pointer
comb = do es <- some simple
          foldl1 combine (map return es)
  where
    combine es e = do e1 <- es
                      e2 <- e
                      lift $ insertNode $ e1 :@: e2

-- | Simplest expressions (constants, builtin functions, lambdas, parenthesized expr., names).
simple :: Parser Pointer
simple = choice $ map try
  [ constant, builtin, lambda, parenthesized, name ]

-- | Lambda abstraction. Syntax: @\\arguments . expression@.
lambda :: Parser Pointer
lambda = do _ <- word "\\"
            d <- lamdef
            e <- lift $ insertNode $ BRef "#lam"
            lift $ insertNode $ Def [d] e False

-- | The Definition in a lambda abstraction.
lamdef :: Parser ( [String] , Pointer )
lamdef = do idfrs <- some identifier >>= return . (:) "#lam"
            _ <- word "."
            e <- expr
            return (idfrs, e)

-- | Identifier.
name :: Parser Pointer
name = do idfr <- identifier
          lift $ insertNode $ BRef idfr

-- | Builtin list function (hd and tl)
builtin :: Parser Pointer
builtin = choice $ map try
  [ do _ <- keyword "hd"
       lift $ insertNode BHd
  , do _ <- keyword "tl"
       lift $ insertNode BTl
  ]

-- | Constant expressions (string, number, boolean, or list @[x1,x2,...]@)
constant :: Parser Pointer
constant = choice $ map try
  [ num, bool, str, list, nil ]

-- | Parenthesized expression.
parenthesized :: Parser Pointer
parenthesized = do _ <- word "("
                   e <- expr
                   _ <- word ")"
                   return e

-- | Numeric literal.
num :: Parser Pointer
num = do i <- natural
         lift $ insertNode $ BInt i

-- | String literal.
str :: Parser Pointer
str = do s <- string
         lift $ insertNode $ BString s

-- | Boolean literal
bool :: Parser Pointer
bool = do b <- boolean
          lift $ insertNode $ BBool b

-- | List literal, @[x1,x2,...]@.
list :: Parser Pointer
list = do _ <- word "["
          choice $ map try
            [ do ls <- listelems
                 _  <- word "]"
                 return ls
            , do _  <- word "]"
                 lift $ insertNode BNil
            ]

-- | The empty list, denoted as @nil@.
nil :: Parser Pointer
nil = do _ <- keyword "nil"
         lift $ insertNode BNil

-- | List elements, separated by commas.
listelems :: Parser Pointer
listelems = do e1 <- expr
               cs <- lift $ insertNode BCons
               choice $ map try
                 [ do _  <- word ","
                      e2 <- listelems
                      e3 <- lift $ insertNode $ cs :@: e1
                      lift $ insertNode $ e3 :@: e2
                 , do nl <- lift $ insertNode BNil
                      e2 <- lift $ insertNode $ cs :@: e1
                      lift $ insertNode $ e2 :@: nl
                 ]

-- | Unary operator (@+@, @-@, or @not@).
prefix :: Parser Pointer
prefix = choice $ map try
  [ do _ <- word "-"
       lift $ insertNode BMinus
  , do _ <- word "+"
       lift $ insertNode BPlus
  , do _ <- keyword "not"
       lift $ insertNode BNot
  ]

-- | Addition and Substraction operators.
addop :: Parser Pointer
addop = choice $ map try
  [ do _ <- word "-"
       lift $ insertNode BSub
  , do _ <- word "+"
       lift $ insertNode BAdd
  ]

-- | Multiplication and Division operators.
mulop :: Parser Pointer
mulop = choice $ map try
  [ do _ <- word "*"
       lift $ insertNode BMul
  , do _ <- word "/"
       lift $ insertNode BDiv
  ]

-- | Comparison operators.
relop :: Parser Pointer
relop = choice $ map try
  [ do _ <- word "<="
       lift $ insertNode BLq
  , do _ <- word "<"
       lift $ insertNode BLt
  , do _ <- word ">="
       lift $ insertNode BGq
  , do _ <- word ">"
       lift $ insertNode BGt
  , do _ <- word "~="
       lift $ insertNode BNq
  , do _ <- word "="
       lift $ insertNode BEq
  ]

-- | Run a given Parser.
parseWith :: Parser Pointer -> String -> St Pointer
parseWith prsr inp = do
  result <- runParserT prsr "Parser.hs" inp
  case result of
    Left err   -> error $ errorBundlePretty err
    Right root -> setParserGraph >> return root

-- | Run the @program@ parser.
parse :: String -> St Pointer
parse = parseWith program


-- * Parsing for the REPL

-- | Program for the repl. Needed, as programs entered in the REPL
-- can be incomplete or empty.
replProgram :: Parser Pointer
replProgram = do _ <- junk
                 p <- choice $ map try [ system, incomplete, nothing ]
                 _ <- junk
                 _ <- eof
                 return p

-- | An incomplete program (only global defs, no expression).
incomplete :: Parser Pointer
incomplete = do ds <- funcdefs
                lift $ insertNode $ Def ds nullPtr True

-- | Am empty program (returns nullPtr).
nothing :: Parser Pointer
nothing = eof >> return nullPtr

-- | Run the @replProgram@ Parser.
replParse :: String -> St Pointer
replParse = parseWith replProgram