module Eval (
    Env,
    builtins,
    eval
) where

import Sexps
import Read

import Data.Maybe (fromMaybe, fromJust, isJust)

{--
 - Eval
 -}

eval :: EnvEvaluator

-- error, let it go through
eval env err@(SError _) = (err, env)

-- self-evaluating expression or a variable
eval env sAtom@(SAtom atom) =
    case atom of
      ASymbol sym | sym == bFalse -> (sAtom, env)
      ASymbol sym -> (symval, env)
        where symval = lookupSymbol env sym
      _ -> (sAtom, env)

-- '() -> nil
eval env (SList []) = (sexpSym bFalse, env)

-- form starting with an atom
eval env (SList ss@((SAtom hd):tl)) =
  case hd of
    etor@(AEvaluator _ _) -> applyEvaluator env "?" etor tl
    ASymbol sym -> case lookup sym specials of
        Just specform -> specform env tl
        Nothing -> applySymbol sym tl env
    _ -> (SError "eval: form must start with a symbol or a form", env)

-- form starting with a lambda
eval env (SList (hd:tl)) | isLambda hd =
    applyLambda env hd tl

-- form starting with a form
eval env (SList ss@((SList hd):tl)) = eval env' (SList (form:tl))
  where (sexpr, env') = eval env (SList hd)
        form = case sexpr of
                SList _ -> SError "eval: the head of form evals to list"
                _ -> sexpr

-- unknown thing
eval env _ = (SError "eval error", env)

{-
 -  Special forms
 -}

specials = [
    ("quote", withEnv evalQuote ),
    ("if",      evalIf          ),
    ("def",     evalDef         ),
    ("lambda",  makeLambda      ),
    ("begin",   evalSeq         )]

evalQuote :: LEvaluator
evalQuote (sexpr:[]) = sexpr
evalQuote _ = SError "evalQuote: too many things to quote"

evalIf, evalDef, evalSeq :: EnvLEvaluator
evalIf env (predf:thenf:elsef:[]) =
    let (p, env') = eval env predf
    in case p of
        SError _ -> (p, env')
        _ -> if isTrue p
             then eval env thenf
             else eval env elsef
evalIf env _ = (SError "evalIf: where is my parts?", env)

evalDef env ((SAtom defatom):defval:[]) =
  case defatom of
    sym@(ASymbol symname) -> (SAtom sym, envAddSymbol symname evaluatedVal env')
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

evalSeq env (sexpr:[]) = eval env sexpr
evalSeq env (this:next) = evalSeq env' next
    where (_, env') = eval env this
evalSeq env _ = (SError "evalSeq: empty sequence?", env)

applySymbol :: String -> [SExpr] -> Env -> (SExpr, Env)
applySymbol symname ss env =
    let sym = lookupSymbol env symname
    in case sym of
          SError _ -> (sym, env)
          SAtom atom -> applyEvaluator env symname atom ss
          SList ((SAtom symLambda):_) -> applyLambda env sym ss
          _ -> (SError "applySymbol: cannot apply a list or whatever", env)

applyEvaluator :: Env -> String -> Atom -> [SExpr] -> (SExpr, Env)
applyEvaluator env _ (AEvaluator evalfun argnum) ss | length ss > argnum =
    (SError $ "applyEvaluator: too many arguments, must be " ++ show argnum, env)
-- partial evaluation:
applyEvaluator env sym etor@(AEvaluator evalfun argnum) ss | length ss < argnum =
    applyLambda env' lambda ss  -- lambda is a lambda-wrapper on evaluator
    where (lambda, env') = makeLambda env (SList largs : lbody : [])
           -- TODO: this is not safe, change to kind of GENSYM:
          largs = map (sexpSym . ("_arg_" ++) . show) [1..argnum]
          lbody = SList (sexpSym sym : largs)
applyEvaluator env _ (AEvaluator evalfun argnum) ss =
    let (ess, env') = evalArgs env ss
    in case filter (isJust.maybeSError) ess of
        (err@(SError _):_) -> (err, env') -- at least one eval(arg) has failed
        [] -> evalfun env' ess      -- eval
applyEvaluator env _ _ _ = (SError "applyEvaluator: cannot apply atom", env)

-- this routine evaluates 'ss' sequentially, accumulating changes to 'env'
evalArgs :: Env -> [SExpr] -> ([SExpr], Env)
evalArgs startenv ss = (tail evalss, last envs)
  where (evalss, envs) = unzip $ scanl evalArg (SList [], startenv) ss
        evalArg (_,env) sexpr = eval env sexpr

{--
 -  Lambdas
 -}
symLambda = ASymbol "lambda"

isLambda :: SExpr -> Bool
isLambda (SList (hd:_)) | isSymbol hd "lambda" = True
isLambda _ = False

lambdaArgs :: SExpr -> [String]
lambdaArgs (SList ((SAtom symLambda):(SList arglist):_:[])) =
        map (fromJust . symbolName) arglist
lambdaArgs _ = []

lambdaBody :: SExpr -> SExpr
lambdaBody (SList ((SAtom a):_:body:[])) | a == symLambda   = body
lambdaBody _ = SError "lambdaBody: error"

lambdaParts :: SExpr -> ([SExpr], SExpr)
lambdaParts (SList ((SAtom a):(SList args):body:[])) | a == symLambda = (args, body)
lambdaParts _ = ([], SError "lambdaParts")

applyLambda :: Env -> SExpr -> [SExpr] -> (SExpr, Env)
applyLambda env func args | length args == length argnames =
    mapSnd stripEnvFrame $ eval fenv (lambdaBody func)
    where fenv = makeEnvWith (zip argnames argvals) env'
          (argvals, env') = evalArgs env args
          argnames = lambdaArgs func
-- partial application: make a lambda with a slice of arguments
--   and with (def arg1 exp1) clauses within body
applyLambda env func args | length args < length arglist =
    makeLambda env $ [SList newargs, SList newbody]
    where (arglist, body) = lambdaParts func
          (argdefs, newargs) = splitAt (length args) arglist
          defs = (sexpSym "begin" : map makeDef (zip argdefs args))
          makeDef (a,d) = SList [sexpSym "def", a, d]
          newbody = case body of
                     SAtom atom -> defs ++ [body]
                     SList form -> case form of
                                    (bg : ss) | isSymbol bg "begin" -> defs ++ ss
                                    _ -> defs ++ [body]
applyLambda env func args = (err, env)
    where err = SError $ "applyLambda: " ++ show (length args) ++
                   " arguments for func/" ++ show (length $ lambdaArgs func)

makeLambda :: EnvLEvaluator
makeLambda env ((SList arglist):expr:[]) | all (isJust.symbolName) arglist =
    (lambda, env) where lambda = SList [SAtom symLambda, SList arglist, expr]
makeLambda env ((SList arglist):body) | all (isJust.symbolName) arglist =
    (lambda, env)
    where lambda = SList [SAtom symLambda, SList arglist, seqExpr]
          seqExpr = SList (sexpSym "begin" : body)
makeLambda env _ =
    (SError "makeLambda: (lambda (arg1 arg2 ...) expr1 expr2 ...)", env)

{--
 - Native functions
 -}

builtins = map (mapSnd (uncurry sexpEtor)) [
    ("+",       (withEnv ntAdd, 2)),
    ("-",       (withEnv ntSub, 2)),
    ("*",       (withEnv ntMul, 2)),
    ("eq",      (withEnv ntEq,  2)),
    ("<",       (withEnv ntLess,2)),

    ("and",     (withEnv ntAnd, 2)),
    ("or",      (withEnv ntOr,  2)),
    ("not",     (withEnv ntNot, 1)),

    ("print",   (withEnv ntPrint,1)),
    ("read",    (withEnv ntRead,1)),
    ("eval",    (ntEval,        1)),

    ("map",     (withEnv ntMap, 2)),
    ("fold",    (withEnv ntFold,3)),
    ("filter",  (withEnv ntFlt, 2)),
    ("cons",    (withEnv ntCons,2)),
    ("car",     (withEnv ntCAR, 1)),
    ("cdr",     (withEnv ntCDR, 1))]


ntAdd, ntMul, ntSub :: LEvaluator
ntAdd ((SAtom ax):(SAtom ay):[]) = ntAdd' ax ay
  where ntAdd' (AInt xi) (AInt yi)      = sexpInt (xi + yi)
        ntAdd' (AFloat xf) (AFloat yf)  = sexpFloat (xf + yf)
        ntAdd' (AString xs) (AString ys)= sexpStr (xs ++ ys)
        ntAdd' _ _ = SError "Add: type mismatch"
ntAdd _ = SError "Add requires two atom arguments"

ntSub ((SAtom ax):(SAtom ay):[]) = ntSub' ax ay
  where ntSub' (AInt x)  (AInt y)     = sexpInt (x - y)
        ntSub' (AFloat x) (AFloat y)  = sexpFloat (x - y)
        ntSub' _ _ = SError "Sub: type mismatch"
ntSub _ = SError "Sub requires two atom arguments"

ntMul ((SAtom ax):(SAtom ay):[]) = ntMul' ax ay
  where ntMul' (AInt xi)   (AInt yi)    = sexpInt (xi * yi)
        ntMul' (AFloat xf) (AFloat yf)  = sexpFloat (xf * yf)
        ntMul' (AString xs) (AInt yi)   =
                       sexpStr $ concat $ replicate (fromInteger yi) xs
        ntMul' _ _ = SError "ntMul: type mismatch"
ntMul _ = SError "Mul requires two atom arguments"

ntEq, ntLess :: LEvaluator
ntEq ((SAtom ax):(SAtom ay):[]) = toBoolSym (ax == ay)
ntEq ((SList lx):(SList ly):[]) = toBoolSym (eqL lx ly)
  where eqL [] [] = True
        eqL _ [] = False
        eqL [] _ = False
        eqL (x:xs) (y:ys) =
            isTrue (ntEq [x,y]) && isTrue (ntEq [SList xs, SList ys])
ntEq _ = SError "EQ requires two arguments"

ntLess ((SAtom ax):(SAtom ay):[]) = toBoolSym $ ntLess' ax ay
  where ntLess' (AInt x) (AInt y) = x < y
        ntLess' (AFloat x) (AFloat y) = x < y
        ntLess' (AString x) (AString y) = x < y
        ntLess' _ _ = False
ntLess _ = SError "don't know how to compare"

ntAnd = undefined
ntOr = undefined
ntNot = undefined
ntPrint = undefined

ntRead (SAtom a:[]) = case a of
    AString str -> readSExpr str
    _ -> SError "read: not a string"
ntRead _ = SError "read: string expected"

ntEval env (sexpr:[]) = eval env' sexpr'
    where (sexpr', env') = eval env sexpr

ntMap = undefined
ntFold = undefined
ntFlt = undefined
ntCons (sexpr:(SList tl):[]) = SList (sexpr:tl)
ntCons _ = SError "CONS arg error"

ntCAR, ntCDR :: LEvaluator
ntCAR ((SList list):[]) = case list of
    (t:_) -> t
    _ -> SError "CAR at empty list"
ntCAR _ = SError "CAR requires one list argument"

ntCDR ((SList list):[]) = case list of
    (_:t) -> SList t
    _ -> SError "CDR: no tail"
ntCDR _ = SError "CDR requires one list argument"


withEnv :: (b -> c) -> a -> b -> (c, a)
withEnv f env x = (f x, env)

withSnd :: b -> a -> (a, b)
withSnd b a = (a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

