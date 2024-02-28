{-|
Module      : Main
Description : Main
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains the main entry point of 
the compiler and the command line interface.

-}

module Main where

import Options.Applicative
    ( (<**>),
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strArgument,
      switch,
      execParser,
      helper,
      Parser, Alternative ((<|>)) )

import Compiler (Args(..), invokeCompiler)

fileParser :: Parser String
fileParser = strArgument (metavar "FILE") <|> pure []

argsParser :: Parser Args
argsParser = Args
      <$> fileParser
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "enable verbose output" )
      <*> switch
          ( long "optimizer"
         <> short 'o'
         <> help "turn on the optimizer")
      <*> switch
          ( long "optimizedCompiler"
         <> short 'c'
         <> help "use optimized compiler")
      <*> switch
          ( long "repl"
         <> short 'i'
         <> help "start the repl" )
      <*> switch
          ( long "showGraphs"
         <> short 'g'
         <> help "output graphs in pdf-files" )

-- | Entry point.
main :: IO ()
main = execParser opts >>= invokeCompiler
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Compile a SASL-program using the following options:"
     <> header "SASL-Compiler CLI" )