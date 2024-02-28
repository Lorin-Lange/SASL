{-|
Module      : Frontend.Primitives
Description : Primitives
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

TODO() : Longer description
-}

module Frontend.Primitives where

import Utilities.State ( St )
import Control.Applicative ()
import Data.Void (Void)
import Data.Char (isDigit, isSpace)
import Text.Megaparsec
    ( anySingle,
      manyTill,
      chunk,
      satisfy,
      single,
      choice,
      many,
      some,
      MonadParsec(try, notFollowedBy),
      ParsecT )

-- | Simple non-transformer Parser type.
type Parser = ParsecT Void String St

-- | Parse whitespace (newlines, tabs, spaces, ...)
whitespace :: Parser String
whitespace = many $ satisfy isSpace

-- | Parse a comment. Either @//...@ or @\/*...*\/@.
comment :: Parser String
comment = choice $ map try
  [ chunk "//" >> manyTill anySingle (chunk "\n")
  , chunk "/*" >> manyTill anySingle (chunk "*/")
  ]

-- | Exhaustively parse whitespace and comments.
junk :: Parser [String]
junk = whitespace >> many (comment >> whitespace)

-- | Parse a given word, and toss the junk it precedes.
word :: String -> Parser String
word s = chunk s >> junk >> return s

-- | Parse a character that allowed for use in identifiers.
idChar :: Parser Char
idChar = satisfy $ flip elem $ ['_'] ++ 
                               ['a'..'z'] ++ 
                               ['A'..'Z'] ++ 
                               ['0'..'9']

-- | Parse a specific keyword, and toss the junk it precedes.
-- Fails if the word to be parsed is followed by an @idChar@.
keyword :: String -> Parser String
keyword s = chunk s >> notFollowedBy idChar >> junk >> return s

-- | List of reserved keywords.
keywords :: [String]
keywords = ["def", "not", "where", "if", "then", "and", "or",
            "else", "true", "false", "nil", "hd", "tl"]

-- | Parses some reserved keyword.
reserved :: Parser String
reserved = choice $ map (try . keyword) keywords

-- | Parses a natural number.
natural :: Parser Integer
natural = do x <- some $ satisfy isDigit
             _ <- junk
             return $ read x

-- | Parses a boolean literal (@true@ or @false@).
boolean :: Parser Bool
boolean = choice $ map try
  [ keyword "true"  >> return True 
  , keyword "false" >> return False
  ]

-- | Parses a string literal (text enclosed in quotation).
string :: Parser String
string = do _   <- single '"'
            str <- many $ satisfy (/= '"')
            _   <- single '"'
            _   <- junk
            return str

-- | Parsed an identifier, that is, some @idChars@.
-- Fails if identifier to be parsed, is a reserved keyword.
identifier :: Parser String
identifier = do notFollowedBy reserved
                idfr <- some idChar
                _    <- junk
                return idfr