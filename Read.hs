module Read (
    topSExprs, readSExpr
) where

import Sexps

import Data.Maybe (fromMaybe, fromJust)
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
sexpParser = squote <|> slist <|> satom <?> "a list or atom"

slist = SList <$> (cSP *> (sexpParser `sepEndBy` seps) <* cCP)
satom = try snum <|> try sstring <|> ssym
snum = do
    s <- many1 symchars
    case reads s :: [(Integer, String)] of
      [(i, "")] -> return $ sexpInt i
      _ -> case reads s :: [(Float, String)] of
        [(f, "")] -> return $ sexpFloat f
        _ -> fail "no int"
sstring = sexpStr <$> (cQu >> quotedChar `manyTill` cQu)
ssym = sexpSym <$> many1 symchars
squote = quote <$> (char '\'' >> sexpParser)

quotedChar = (char '\\' >> ((oneOf (fst$unzip escapes) >>= return . fromJust . flip lookup escapes) <|> anyChar)) <|> anyChar
symchars = noneOf "'(); \n\t"

seps = spaces *> optional comment <* spaces
comment = char ';' >> many (noneOf "\n")
escapes = zip "ntr" "\n\t\r"

cQu = char '"'
cSP = char '('
cCP = char ')'

fromEither = flip either id
