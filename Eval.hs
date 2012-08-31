module Eval (
    Env,
    initEnv,
    makeEnvWith,
    makeEmptyEnv,
    eval
) where

import AST
import LexAn

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M

{-
 - Environments
 -}

type Frame = M.Map String SExpr

data Env = Env {
    frames :: [Frame]
}

makeEmptyEnv :: Env -> Env
makeEmptyEnv env = env { frames = (M.empty : frames env) }

makeEnvWith :: [(String, SExpr)] -> Env -> Env
makeEnvWith vs env = env { frames = (M.fromList vs : frames env) }

initEnv :: Env
initEnv = Env [ M.fromList builtins ]

builtins = [
    ("+",       SAtom $ AtomEvaluator nativeAdd 2),
    ("*",       SAtom $ AtomEvaluator nativeMul 2)]

{--
 - Native functions
 -}

nativeAdd :: Evaluator
nativeAdd ((SAtom ax):(SAtom ay):[]) = nativeAdd' ax ay
  where nativeAdd' (AtomInt xi) (AtomInt yi)      = SAtom $ AtomInt (xi + yi)
        nativeAdd' (AtomFloat xf) (AtomFloat yf)  = SAtom $ AtomFloat (xf + yf)
        nativeAdd' (AtomString xs) (AtomString ys)= SAtom $ AtomString (xs ++ ys)
        nativeAdd' _ _ = SError "nativeAdd: type mismatch"
nativeAdd _ = SError "nativeAdd requires two atom arguments"

nativeMul :: Evaluator
nativeMul ((SAtom ax):(SAtom ay):[]) = nativeMul' ax ay
  where nativeMul' (AtomInt xi)   (AtomInt yi)    = SAtom $ AtomInt (xi * yi)
        nativeMul' (AtomFloat xf) (AtomFloat yf)  = SAtom $ AtomFloat (xf * yf)
        nativeMul' (AtomString xs) (AtomInt yi)   =
                       SAtom $ AtomString $ concat $ replicate (fromInteger yi) xs
        nativeMul' _ _ = SError "nativeMul: type mismatch"
nativeMul _ = SError "nativeMul requires two atom arguments"

{--
 - Eval
 -}

eval :: Env -> SExpr -> (SExpr, Env)

-- error, let it go through
eval env err@(SError _) = (err, env)

-- self-evaluating expression or a variable
eval env sAtom@(SAtom atom) =
    case atom of
      AtomSymbol "nil" -> (sAtom, env)
      AtomSymbol sym -> (symval, env)
        where symval = lookupSymbol env sym
      _ -> (sAtom, env)

-- '() -> nil
eval env (SList []) = (SAtom $ AtomSymbol "nil", env)

-- form starting with an atom
eval env (SList ss@((SAtom hd):tl)) =
    case hd of
      etor@(AtomEvaluator _ _) -> (applyEvaluator etor tl, env)
      AtomSymbol "quote" -> (val, env)
        where val = fromMaybe (head tl) $ assertFormLength 2 ss
      AtomSymbol "if" -> fromMaybe result $ withEnv env $ assertFormLength 4 ss
        where (predf:(thenf:(elsef:_))) = tl
              result = (evalIf env predf thenf elsef)
      AtomSymbol "car" -> fromMaybe (car, env') $ withEnv env $ assertFormLength 2 ss
        where (arg, env') = eval env (head tl)
              car = case arg of
                SList (c:_) -> c
                _ -> SError "Non-empty list expected"
      AtomSymbol "cdr" -> fromMaybe (cdr, env') $ withEnv env $ assertFormLength 2 ss
        where (arg, env') = eval env (head tl)
              cdr = case arg of
                SList (_:t) -> SList t
                _ -> SError "No list tail"
      AtomSymbol sym -> applySymbol sym tl env
      _ -> (SError "eval: form must start with a symbol or a form", env)

-- form starting with a form
eval env (SList ss@((SList hd):tl)) = eval env' (SList (form:tl))
  where (sexpr, env') = eval env (SList hd)
        form = case sexpr of
                SList _ -> SError "eval: the head of form evals to list"
                _ -> sexpr

-- unknown thing
eval env _ = (SError "eval error", env)

evalIf :: Env -> SExpr -> SExpr -> SExpr -> (SExpr, Env)
evalIf env predicate thenf elsef =
    let (p, env') = eval env predicate
    in case p of
        SError _ -> (p, env')
        _ -> if isTrue p then eval env thenf else eval env elsef

lookupSymbol :: Env -> String -> SExpr       -- maybe SError
lookupSymbol env sym = lookupInFrames (frames env) sym
    where lookupInFrames [] sym     = SError $ "Symbol " ++ show sym ++ " not found"
          lookupInFrames (f:fs) sym = fromMaybe (lookupInFrames fs sym) $ M.lookup sym f

envAddSymbol :: String -> SExpr -> Env -> Env
envAddSymbol name val env = env

applySymbol :: String -> [SExpr] -> Env -> (SExpr, Env)
applySymbol symname ss env =
    let sym = lookupSymbol env symname
    in case sym of
          SError _ -> (sym, env)
          SAtom atom -> (applyEvaluator atom ss, env)
          _ -> (SError "applySymbol: cannot apply a list or whatever", env)

applyEvaluator :: Atom -> [SExpr] -> SExpr
applyEvaluator (AtomEvaluator evalfun argnum) ss =
    if argnum == length ss then evalfun ss
    else if argnum > length ss
      then SError "applySymbol: partial application not implemented yet"
      else SError $"applySymbol: too many arguments, must be " ++ show argnum
applyEvaluator _ _ = SError "applySymbol: cannot apply atom"

assertFormLength :: Int -> [SExpr] -> Maybe SExpr
assertFormLength n ss =
    if length ss == n then Nothing
    else Just $ SError $ "invalid number of arguments, must be " ++ show n

withEnv :: Env -> Maybe SExpr -> Maybe (SExpr, Env)
withEnv env = fmap (flip (,) env)

