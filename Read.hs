module Read (
    topSExprs, readSExpr
) where

import Sexps

import Control.Monad (liftM)
import Control.Applicative hiding ((<|>), optional, many)
import Text.Parsec
import Text.ParserCombinators.Parsec (CharParser(..))

readSExpr :: String -> SExpr
readSExpr = fromEither reportError . parse (seps *> sexpParser <* seps <* eof) ""

topSExprs :: String -> [SExpr]
topSExprs = fromEither reportLError . parse (sexpParser `sepEndBy` seps) ""

reportError = SError . show -- const (SError "read error")
reportLError = const [SError "read error"]

sexpParser :: CharParser st SExpr
sexpParser = squote <|> slist <|> satom <?> "a list or atom expected"

slist = SList <$> (opPar *> (sexpParser `sepEndBy` seps) <* clPar)
satom = try snum <|> try sstring <|> ssym
snum = do
    s <- many1 symchars
    case reads s :: [(Integer, String)] of
      [(i, "")] -> return $ sexpInt i
      _ -> case reads s :: [(Float, String)] of
        [(f, "")] -> return $ sexpFloat f
        _ -> fail "no int"
sstring = sexpStr <$> (quCh >> manyTill quotedChar quCh)
ssym = sexpSym <$> many1 symchars
squote = quote <$> (char '\'' >> sexpParser)

quotedChar = noneOf "\\" <|> try (string "\\\"" >> return '"')
symchars = noneOf "'(); \n\t"

seps = spaces *> optional comment <* spaces
comment = char ';' >> many (noneOf "\n")
escapes = zip "ntr" "\n\t\r"

quCh = char '"'
opPar = char '('
clPar = char ')'

fromEither = flip either id
