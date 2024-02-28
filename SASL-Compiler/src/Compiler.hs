{-|
Module      : Compiler
Description : Compiler
Copyright   : (c) Lorin Lange, 2022
                  Simon Klingler, 2022
Maintainer  : lorin.lange@student.uni-tuebingen.de, 
              simon.klingler@student.uni-tuebingen.de
Stability   : experimental

This module contains the functions
to invoke the compiler and the REPL.
-}

module Compiler where

import Frontend.Parser (parse, replParse)
import Backend.Compiler (compile)
import Backend.OptimizedCompiler (optimizedCompile)
import Backend.ReductionMachine (reduce)
import Backend.Optimizer (optimize)
import Backend.Printer (getResult)
import Utilities.Types (Pointer, nullPtr)
import Control.Monad.State
 (execState, runState, evalState, MonadTrans (lift))
import Utilities.State
 (St, initState, printState, printDefinitions, getGraphs,
  StateI, merge, executable, clearProg, garbageCollect)
import System.CPUTime (getCPUTime)
import Control.Exception (catch, SomeException)
import System.Console.Haskeline
 (InputT, runInputT, defaultSettings, getInputLine)
import SASLPrelude (prelude)
import Utilities.GraphVisualisation (run)
import Control.Monad (void)

type Program = (Pointer, StateI)

-- | CLI arguments type.
data Args = Args { path              :: String -- ^ .sasl filepath.
                 , verbose           :: Bool   -- ^ verbose output switch.
                 , optimizer         :: Bool   -- ^ optimizer switch.
                 , optimizedCompiler :: Bool   -- ^ optimized compiler switch.
                 , repl              :: Bool   -- ^ repl switch.
                 , showGraphs        :: Bool   -- ^ graph-output switch
                 }

-- | Pass program through a sequence of individual compilation steps.
-- Choose optimiztion level with CLI-switches.
pipeline :: Args -> Pointer -> St Pointer
pipeline Args { optimizer = o, optimizedCompiler = c } ptr =
  switchCompiler c ptr >>= switchOptimizer o >>= reduce

-- | Choose compilation method. Used in @pipeline@.
switchCompiler :: Bool -> Pointer -> St Pointer
switchCompiler True  = optimizedCompile
switchCompiler False = compile

-- | Choose optimization method. Used in @pipline@.
switchOptimizer :: Bool -> Pointer -> St Pointer
switchOptimizer True  = optimize
switchOptimizer False = return

-- | Run default compiler with the prelude and an input expression, return the result as a String.
-- @runCompiler@ and @runCompilerWithArgs@ are used for testing purposes only.
runCompiler :: String -> String
runCompiler input = do
  let args = Args "" False True True False False
  runCompilerWithArgs args input

-- | Like @runCompiler@ but with custom args.
runCompilerWithArgs :: Args -> String -> String
runCompilerWithArgs args input = do
  let (ptr, state) = runState (parse $ prelude ++ input) initState
  getResult $ execState (pipeline args ptr) state

-- | Execute a program from a file or start the REPL, depending on the REPL-switch.
invokeCompiler :: Args -> IO ()
invokeCompiler args@Args { path = "", repl = True }  = startRepl args
invokeCompiler args@Args { repl = True }             = startReplWithFile args
invokeCompiler args@Args { repl = False }            = executeFile args

-- | Execute a program from a file.
executeFile :: Args -> IO ()
executeFile args@Args { path = p } = do
  input <- readFile p
  executeProg args $ runState (parse input) initState

-- | Greet the user, and start the REPL.
startRepl :: Args -> IO ()
startRepl args = do
  putStrLn "\nWelcome to the SASL-REPL. Enter :h for help.\n"
  prog <- executeRepl args (nullPtr, initState) prelude
  runInputT defaultSettings $ loop args prog

-- | Greet the user, start the REPL, and load a file into it.
startReplWithFile :: Args -> IO ()
startReplWithFile args@Args { path = p } = do
  putStrLn "\nWelcome to the SASL-REPL. Enter :h for help."
  input <- readFile p
  prog  <- executeRepl args (nullPtr, initState) (prelude ++ "\n" ++ input)
  putStrLn $ p ++ " loaded.\n"
  runInputT defaultSettings $ loop args prog

-- | Execute a program.
executeProg :: Args -> Program -> IO ()
executeProg args@Args { verbose = True }  = executeVerbose args
executeProg args@Args { verbose = False } = executeNonVerbose args

-- | Execute a program verbosely.
executeVerbose :: Args -> Program -> IO ()
executeVerbose args@Args { showGraphs = g } (ptr, state) = do
  start <- getCPUTime
  let state1 = execState (pipeline args ptr) state
  printState state1
  end   <- getCPUTime
  putStr "Time: "
  putStrLn $ show (fromIntegral (end - start) / (10**12) :: Double) ++ "s"
  putStrLn $ getResult state1
  writeGraphs g state1

-- | Execute a program non-verbosely.
executeNonVerbose :: Args -> Program -> IO ()
executeNonVerbose args@Args { showGraphs = g } (ptr, state) = do
  let state1 = execState (pipeline args ptr) state
  putStrLn $ getResult state1
  writeGraphs g state1

-- | Show graphs depending on the boolean argument.
writeGraphs :: Bool -> StateI -> IO ()
writeGraphs False _     = return ()
writeGraphs True  state = do
  let (g1, g2, g3, g4) = evalState getGraphs state
  run "parserGraph.pdf"    g1
  run "compilerGraph.pdf"  g2
  run "optimizerGraph.pdf" g3
  run "reductionGraph.pdf" g4

-- | The SASL-REPL loop.
loop :: Args -> Program -> InputT IO ()
loop args prog@(ptr, state) = do
  input <- getReplInput
  case processInput input of
    (":q", _)    -> void $ lift $ putStrLn "Goodbye, have a nice day!"
    (":h", _)    -> lift printHelp >> loop args prog
    (":list", _) -> lift (printDefinitions ptr state) >> loop args prog
    (w, inp)     -> executeAndLoop (w ++ " " ++ inp)
 where
  executeAndLoop input = do
    lift (catch (executeRepl args prog input) (handler prog)) >>= loop args

-- | Get input from the repl. 
-- Either directly, or via @multilineInput@ (@:{ ... :}@), or from a specified file (@:l ...@).
getReplInput :: InputT IO String
getReplInput = do
  minput <- getInputLine "SASL> "
  case processInput <$> minput of
    Just (":l", file) -> lift $ catch (readFile file) fileHandler
    Just (":{", _)    -> multilineInput ""
    Just (w, inp)     -> return $ w ++ " " ++ inp
    _ -> undefined

-- | Read several lines of input from the REPL.
multilineInput :: String -> InputT IO String
multilineInput inp = do
  minput <- getInputLine "SASL| "
  case processInput <$> minput of
    Just (":}", _) -> return inp
    Just (w, new)  -> multilineInput $ inp ++ "\n" ++ w ++ " " ++ new
    _ -> undefined

-- | Parse and execute a program in the REPL. Parsing, and merging of ASTs happens here.
executeRepl :: Args -> Program -> String -> IO Program
executeRepl args (ptr1, state1) input = do
  let (ptr2, state2) = runState (replParse input) state1
  let (ptr3, state3) = runState (merge ptr1 ptr2) state2
  let exe = evalState (executable ptr3) state3
  if exe then do
    _ <- executeProg args (ptr3, state3)
    return $ runState (garbageCollect ptr3 >>= clearProg) state3
  else
    return (ptr3, state3)

-- | Process input for the REPL. Returns tuple @(first word, rest of input)@.
processInput :: String -> (String, String)
processInput inp = case words inp of
                     []   -> ("", "")
                     w:ws -> (w, unwords ws)

-- | Print help for the available REPL-commands.
printHelp :: IO ()
printHelp = do
  putStrLn "\nCommands available in the REPL:\n"
  putStrLn $ ":h"        ++ "\t\tshow help"
  putStrLn $ ":q"        ++ "\t\tquit"
  putStrLn $ ":l <path>" ++ "\tload a sasl-file"
  putStrLn $ ":{"        ++ "\t\tenter multiline-input"
  putStrLn $ ":}"        ++ "\t\texit multiline-input"
  putStrLn $ ":list"     ++ "\t\tlist all currently defined functions"
  putStrLn $ "<prog>"    ++ "\t\texecute program"
  putStrLn ""

-- | Error handler for loading files in the REPL.
fileHandler :: SomeException -> IO String
fileHandler err = putStr "ERROR: " >> print err >> return ""

-- | Error handler for executing programs in the REPL.
handler :: Program -> SomeException -> IO Program
handler (ptr, state) err = putStr "ERROR: " >> print err >> return (ptr, state)