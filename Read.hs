module Read (
    topSExprs, readSExpr
) where

import Sexps

import Control.Monad (liftM)
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.ParserCombinators.Parsec (CharParser(..))

readSExpr :: String -> SExpr
readSExpr = parseToSexpr . parse sexpParser ""

topSExprs :: String -> [SExpr]
topSExprs [] = []
topSExprs s = parseToLSexpr $ parse (sexpParser `sepEndBy` spaces) "" s

parseToSexpr = fromEither (SError . show)
parseToLSexpr = fromEither ((:[]) . SError . show)

sexpParser :: CharParser st SExpr
sexpParser = squote <|> slist <|> satom <?> "Not a list or atom"

slist = SList <$> (opPar *> (sexpParser `sepEndBy` spaces) <* clPar)
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

-- TODO : comments
quotedChar = noneOf "\\" <|> try (string "\\\"" >> return '"') 
symchars = noneOf " \n\t()"

escapes = zip "ntr" "\n\t\r"

quCh = char '"'
opPar = char '('
clPar = char ')'

fromEither = flip either id
