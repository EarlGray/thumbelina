module AST (
    SExpr(..), Atom(..), LEvaluator,

    topSExprs, readSExpr, makeSExp,
    sexpInt, sexpFloat, sexpStr, sexpSym, sexpEtor,
    symbolName, isSymbol, quote, maybeSError,
    isTrue, bTrue, bFalse, toBoolSym
) where

import LexAn

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

readSExpr :: String -> SExpr
readSExpr s = let (se, ls) = makeSExp $ lexicalAnalyzer s
              in if null ls then se else SError "syntax error"

topSExprs :: [Lexeme] -> [SExpr]
topSExprs ls = topSExprs' [] ls
    where
    topSExprs' ss [] = reverse ss
    topSExprs' ss ls =
      let (s, ls') = makeSExp ls
      in topSExprs' (s:ss) ls'

makeSExp :: [Lexeme] -> (SExpr, [Lexeme])
makeSExp [] = (SError "no lexemes", [])
makeSExp (l:ls) =
    case l of
      LInt i -> (SAtom $ AInt i, ls)
      LFloat f -> (SAtom $ AFloat f, ls)
      LString s -> (SAtom $ AString s, ls)
      LSymbol sym -> (SAtom $ ASymbol sym, ls)
      LSP -> makeSList ls (SList [])
      LQuote -> let (q, ls') = makeSExp ls
                in (quote q, ls')
      _ -> let err = SError $ "Syntax error: can't use lexeme " ++ show l
           in (err, ls)

makeSList :: [Lexeme] -> SExpr -> (SExpr, [Lexeme])
makeSList [] sexp = (SError "no lexemes", [])
makeSList (LCP : ls) (SList ss) = ((SList $ reverse ss), ls)
makeSList ls (SList ss) =
    let (item, ls') = makeSExp ls
    in makeSList ls' $ SList (item:ss)

