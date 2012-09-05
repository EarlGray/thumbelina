module Sexps (
    SExpr(..), Atom(..), LEvaluator,

    sexpInt, sexpFloat, sexpStr, sexpSym, sexpEtor,
    symbolName, isSymbol, quote, maybeSError,
    isTrue, bTrue, bFalse, toBoolSym,

    EnvEvaluator, EnvLEvaluator, Env,
    makeEmptyEnv, makeEnvWith, envAddSymbol, lookupSymbol, stripEnvFrame, initEnv
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data SExpr = SList [SExpr]
           | SAtom Atom
           | SError String

data Atom = AInt Integer
          | AFloat Float
          | AString String
          | ASymbol String
          | AEvaluator EnvLEvaluator Int  -- native form, number of arguments

type LEvaluator = ([SExpr] -> SExpr)

bTrue, bFalse :: String
bTrue = "t"
bFalse = "nil"

instance Show SExpr where
    show (SError s) = "*** " ++ s
    show (SAtom atom) = show atom
    show (SList ss) = "(" ++ (intercalate " " $ map show ss) ++ ")"

instance Show Atom where
    show (AInt i) = show i
    show (AFloat f) = show f
    show (AString s) = show s
    show (ASymbol sym) = sym
    show (AEvaluator _ argnum) = "#builtin/" ++ show argnum

instance Eq Atom where
    (AInt x) == (AInt y)        = x == y
    (AFloat x) == (AFloat y)    = x == y
    (AString x) == (AString y)  = x == y
    (ASymbol x) == (ASymbol y)  = x == y
    _ == _ = False

maybeSError :: SExpr -> Maybe String
maybeSError s = case s of
    SError err -> Just err
    _ -> Nothing

sexpInt i   = SAtom $ AInt i
sexpFloat f = SAtom $ AFloat f
sexpStr s   = SAtom $ AString s
sexpSym sym = SAtom $ ASymbol sym
sexpEtor f n = SAtom $ AEvaluator f n

symbolName :: SExpr -> Maybe String
symbolName (SAtom a) = case a of
   ASymbol sym -> Just sym
   _ -> Nothing
symbolName _ = Nothing

isSymbol sym name = symbolName sym == Just name

toBoolSym b = sexpSym $ if b then bTrue else bFalse

isTrue :: SExpr -> Bool
isTrue (SList []) = False
isTrue sym | symbolName sym == Just bFalse = False
isTrue _ = True

quote :: SExpr -> SExpr
quote sexp = SList [sexpSym "quote", sexp]

{-
 - Environments
 -}

type Frame = M.Map String SExpr

data Env = Env {
    frames :: [Frame]
}

type EnvEvaluator = (Env -> SExpr -> (SExpr, Env))
type EnvLEvaluator = (Env -> [SExpr] -> (SExpr, Env))

makeEmptyEnv :: Env -> Env
makeEmptyEnv env = env { frames = (M.empty : frames env) }

makeEnvWith :: [(String, SExpr)] -> Env -> Env
makeEnvWith vs env = env { frames = (M.fromList vs : frames env) }

initEnv :: [(String, SExpr)] -> Env
initEnv vs = Env [ M.fromList vs ]

lookupSymbol :: Env -> String -> SExpr       -- maybe SError
lookupSymbol env sym = lookupInFrames (frames env) sym
  where lookupInFrames [] sym     = SError $ "Symbol " ++ show sym ++ " not bound"
        lookupInFrames (f:fs) sym = fromMaybe (lookupInFrames fs sym) $ M.lookup sym f

stripEnvFrame :: Env -> Env
stripEnvFrame env = env { frames = (tail $ frames env) }

envAddSymbol :: String -> SExpr -> Env -> Env
envAddSymbol name val env = Env (updatedFrame:others)
  where (frame:others) = frames env
        updatedFrame = M.insert name val frame

instance Show Env where
    show env = show (frames env)

