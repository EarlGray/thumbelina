module Sexps (
    SExpr(..), Atom(..), LEvaluator,

    sexpInt, sexpFloat, sexpStr, sexpSym, sexpEtor,
    symbolName, isSymbol, quote, maybeSError,
    isTrue, bTrue, bFalse, toBoolSym
) where

import Data.List (intercalate)

data SExpr = SList [SExpr]
           | SAtom Atom
           | SError String

data Atom = AInt Integer
          | AFloat Float
          | AString String
          | ASymbol String
          | AEvaluator LEvaluator Int  -- native form, number of arguments

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
isTrue sym | symbolName sym == Just "nil" = False
isTrue _ = True

quote :: SExpr -> SExpr
quote sexp = SList [sexpSym "quote", sexp]
