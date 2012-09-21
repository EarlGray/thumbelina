module Main where

import Sexps
import Read
import Eval (eval, builtins)

import Data.Maybe (isJust)
import System.IO

commands = "Commands are:\n\
\  :q    -- quit\n\
\  :load file.lisp\n\
\  :env  -- show the envirionment"

cmd :: Env -> [String] -> IO ()
cmd env args = do
  case args!!0 of
    "q" -> return ()
    "load" -> do
        file <- readFile $ args!!1
        let ss = SList (sexpSym "seq" : topSExprs file)
        let (out, env') = eval env ss
        print $ if (isJust$maybeSError out) then out else sexpSym "ok"
        repl env'
    "env" -> print env >> repl env
    _ -> putStrLn commands >> repl env

repl :: Env -> IO ()
repl env = do
    putStr ">> " >> hFlush stdout
    line <- getLine
    case line of
      "" -> repl env
      (':':tl) -> cmd env (words tl)
      _ -> do
        let inp = readSExpr line
        let (out, env') = env `seq` eval env inp
        print out
        repl env'

sayHello = do
    putStrLn "Welcome to Thumbelina Scheme"
    putStrLn commands

main :: IO ()
main = do
    sayHello
    repl $ initEnv builtins
