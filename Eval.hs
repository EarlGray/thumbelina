module Eval (
    Env,
    initEnv,
    makeEnvWith,
    makeEmptyEnv,
    eval
) where

import AST
import LexAn

import Data.Maybe (fromMaybe, fromJust, isJust)
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

lookupSymbol :: Env -> String -> SExpr       -- maybe SError
lookupSymbol env sym = lookupInFrames (frames env) sym
  where lookupInFrames [] sym     = SError $ "Symbol " ++ show sym ++ " not found"
        lookupInFrames (f:fs) sym = fromMaybe (lookupInFrames fs sym) $ M.lookup sym f

stripEnvFrame :: Env -> Env
stripEnvFrame env = env { frames = (tail $ frames env) }

envAddSymbol :: String -> SExpr -> Env -> Env
envAddSymbol name val env = Env (updatedFrame:others)
  where (frame:others) = frames env
        updatedFrame = M.insert name val frame

etor :: Evaluator -> Int -> SExpr
etor f n = SAtom $ AtomEvaluator f n

builtins = [
    ("+",       etor ntAdd 2),
    ("-",       etor ntSub 2),
    ("*",       etor ntMul 2),
    ("eq",      etor ntEq 2),
    ("car",     etor ntCAR 1),
    ("cdr",     etor ntCDR 1)]


{--
 - Native functions
 -}

ntAdd, ntMul, ntSub :: Evaluator
ntAdd ((SAtom ax):(SAtom ay):[]) = ntAdd' ax ay
  where ntAdd' (AtomInt xi) (AtomInt yi)      = SAtom $ AtomInt (xi + yi)
        ntAdd' (AtomFloat xf) (AtomFloat yf)  = SAtom $ AtomFloat (xf + yf)
        ntAdd' (AtomString xs) (AtomString ys)= SAtom $ AtomString (xs ++ ys)
        ntAdd' _ _ = SError "Add: type mismatch"
ntAdd _ = SError "Add requires two atom arguments"

ntSub ((SAtom ax):(SAtom ay):[]) = ntSub' ax ay
  where ntSub' (AtomInt x)  (AtomInt y)     = SAtom $ AtomInt (x - y)
        ntSub' (AtomFloat x) (AtomFloat y)  = SAtom $ AtomFloat (x - y)
        ntSub' _ _ = SError "Sub: type mismatch"
ntSub _ = SError "Sub requires two atom arguments"

ntMul ((SAtom ax):(SAtom ay):[]) = ntMul' ax ay
  where ntMul' (AtomInt xi)   (AtomInt yi)    = SAtom $ AtomInt (xi * yi)
        ntMul' (AtomFloat xf) (AtomFloat yf)  = SAtom $ AtomFloat (xf * yf)
        ntMul' (AtomString xs) (AtomInt yi)   =
                       SAtom $ AtomString $ concat $ replicate (fromInteger yi) xs
        ntMul' _ _ = SError "ntMul: type mismatch"
ntMul _ = SError "Mul requires two atom arguments"

ntEq :: Evaluator
ntEq ((SAtom ax):(SAtom ay):[]) =
    toSymbol $ if ntEq' ax ay then bTrue else bFalse
  where ntEq' (AtomInt x)       (AtomInt y)     = x == y
        ntEq' (AtomFloat x)     (AtomFloat y)   = x == y
        ntEq' (AtomString x) (AtomString y)     = x == y
        ntEq' (AtomSymbol x) (AtomSymbol y)     = x == y
        ntEq' _ _ = False
ntEq ((SList lx):(SList ly):[]) =
    toSymbol $ if eqL lx ly then bTrue else bFalse
  where eqL [] [] = True
        eqL _ [] = False
        eqL [] _ = False
        eqL (x:xs) (y:ys) =
            isTrue (ntEq (x:y:[])) && isTrue (ntEq [SList xs, SList ys])
ntEq _ = SError "EQ requires two arguments"

ntCAR, ntCDR :: Evaluator
ntCAR ((SList list):[]) = case list of
    (t:_) -> t
    _ -> SError "CAR at empty list"
ntCAR _ = SError "CAR requires one list argument"

ntCDR ((SList list):[]) = case list of
    (_:t) -> SList t
    _ -> SError "CDR: no tail"
ntCDR _ = SError "CDR requires one list argument"

{--
 - Eval
 -}

type EnvEvaluator = (Env -> SExpr -> (SExpr, Env))

eval :: EnvEvaluator

-- error, let it go through
eval env err@(SError _) = (err, env)

-- self-evaluating expression or a variable
eval env sAtom@(SAtom atom) =
    case atom of
      AtomSymbol sym | sym == bFalse -> (sAtom, env)
      AtomSymbol sym -> (symval, env)
        where symval = lookupSymbol env sym
      _ -> (sAtom, env)

-- '() -> nil
eval env (SList []) = (toSymbol bFalse, env)

-- form starting with an atom
eval env (SList ss@((SAtom hd):tl)) =
  case hd of
    etor@(AtomEvaluator _ _) -> (applyEvaluator etor tl env, env)
    -- special forms
    AtomSymbol "quote" -> (evalQuote tl, env)
    AtomSymbol "if" -> evalIf env tl
    AtomSymbol "def" -> evalDef env tl
    AtomSymbol "lambda" -> makeLambda env tl
    AtomSymbol "begin" -> evalSeq env tl

    AtomSymbol sym -> applySymbol sym tl env
    _ -> (SError "eval: form must start with a symbol or a form", env)

-- form starting with a form
eval env (SList ss@((SList hd):tl)) = eval env' (SList (form:tl))
  where (sexpr, env') = eval env (SList hd)
        form = case sexpr of
                SList ((SAtom symLambda):_) -> SError "eval: TODO lambda evaluation"
                SList _ -> SError "eval: the head of form evals to list"
                _ -> sexpr

-- unknown thing
eval env _ = (SError "eval error", env)


evalQuote :: [SExpr] -> SExpr
evalQuote (sexpr:[]) = sexpr
evalQuote _ = SError "evalQuote: too many things to quote"

evalIf :: Env -> [SExpr] -> (SExpr, Env)
evalIf env (predf:thenf:elsef:[]) =
    let (p, env') = eval env predf
    in case p of
        SError _ -> (p, env')
        _ -> if isTrue p
             then eval env thenf
             else eval env elsef
evalIf env _ = (SError "evalIf: where is my parts?", env)

evalDef :: Env -> [SExpr] -> (SExpr, Env)
evalDef env ((SAtom defatom):defval:[]) =
  case defatom of
    sym@(AtomSymbol symname) -> (SAtom sym, envAddSymbol symname evaluatedVal env')
    _ -> (SError "evalDef: a symbol must be defined", env)
  where (evaluatedVal, env') = eval env defval
evalDef env ((SList defun):defbody) =
  case defun of
    (atom@(SAtom _):args) ->
      case symbolName atom of
        Nothing -> (SError "function name must be a symbol", env)
        Just sym -> (atom, envAddSymbol sym lambda env')
          where (lambda, env') = makeLambda env (SList args : defbody)
    _ -> (SError "function name must be an atom", env)

evalDef env _ = (SError "evalDef: def error", env)

evalSeq :: Env -> [SExpr] -> (SExpr, Env)
evalSeq env (sexpr:[]) = eval env sexpr
evalSeq env (this:next) = evalSeq env' next
    where (_, env') = eval env this
evalSeq env _ = (SError "evalSeq: empty sequence?", env)

applySymbol :: String -> [SExpr] -> Env -> (SExpr, Env)
applySymbol symname ss env =
    let sym = lookupSymbol env symname
    in case sym of
          SError _ -> (sym, env)
          SAtom atom -> (applyEvaluator atom ss env, env)
          SList ((SAtom symLambda):_) -> applyLambda env sym ss
          _ -> (SError "applySymbol: cannot apply a list or whatever", env)

applyEvaluator :: Atom -> [SExpr] -> Env -> SExpr
applyEvaluator (AtomEvaluator evalfun argnum) ss env =
  let ess = map (fst . eval env) ss -- TODO: env may change
  in  if argnum == length ess then evalfun ess
      else if argnum > length ess
        then SError "applySymbol: partial application not implemented yet"
        else SError $"applySymbol: too many arguments, must be " ++ show argnum
applyEvaluator _ _ _ = SError "applySymbol: cannot apply atom"


{--
 -  Lambdas
 -}
symLambda = AtomSymbol "lambda"

isLambda :: SExpr -> Bool
isLambda (SList ((SAtom symLambda):_)) = True
isLambda _ = False

lambdaArgs :: SExpr -> [String]
lambdaArgs (SList ((SAtom symLambda):(SList arglist):_:[])) =
        map (fromJust . symbolName) arglist
lambdaArgs _ = []

lambdaBody :: SExpr -> SExpr
lambdaBody (SList ((SAtom symLambda):_:body:[])) = body
lambdaBody _ = SError "lambdaBody: error"

applyLambda :: Env -> SExpr -> [SExpr] -> (SExpr, Env)
applyLambda env func args | length (args) == length argnames =
    mapSnd stripEnvFrame $ eval fenv (lambdaBody func)
    where argvals = map (fst . eval env) args
          argnames = lambdaArgs func
          fenv = makeEnvWith (zip argnames argvals) env
applyLambda env func args = (err, env)
    where err = SError $ "applyLambda: " ++ show (length args) ++
                   " arguments for func/" ++ show (length $ lambdaArgs func)

makeLambda :: Env -> [SExpr] -> (SExpr, Env)
makeLambda env ((SList arglist):expr:[]) | all (isJust.symbolName) arglist =
    (lambda, env) where lambda = SList ((SAtom symLambda):(SList arglist):expr:[])
makeLambda env ((SList arglist):body) | all (isJust.symbolName) arglist =
    (lambda, env)
    where lambda = SList ((SAtom symLambda):(SList arglist):seqExpr:[])
          seqExpr = SList [toSymbol "begin", SList body]
makeLambda env _ =
    (SError "makeLambda: (lambda (arg1 arg2 ...) expr1 expr2 ...)", env)


withEnv :: Env -> Maybe SExpr -> Maybe (SExpr, Env)
withEnv env = fmap (flip (,) env)

withSnd :: b -> a -> (a, b)
withSnd b a = (a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

