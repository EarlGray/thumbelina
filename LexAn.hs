module LexAn (
    Lexeme(..),
    lexicalAnalyzer
) where

import Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)

{-
 -  Lexical analyzersym
 -}

data Lexeme
    = LSP     -- start parenthesis
    | LCP     -- closing prntsis.
    | LQuote
    | LInt Integer
    | LFloat Float
    | LSymbol String
    | LString String
    | LError

data LState
    = LSWhitespace
    | LSSymbol Char
    | LSWord String
    | LSInString String Bool -- string; is the character escaped?
    | LSString String
    | LSComment
    | LSError
    | LSEOF

instance Show Lexeme where
    show LSP = "<(> "
    show LCP = "<)> "
    show LQuote = "<'> "
    show (LInt int) = "<I@" ++ show int ++ "> "
    show (LFloat f) = "<F@" ++ show f ++ "> "
    show (LSymbol s) = "<S@" ++ show s ++ "> "
    show (LString s) = "<\"" ++ show s ++ "\"> "
    show LError = "ERROR"

instance Eq LState where
    LSWhitespace == LSWhitespace    = True
    LSSymbol _ == LSSymbol _        = True
    LSWord _ == LSWord _            = True
    LSInString _ _ == LSInString _ _ = True
    LSString _   == LSString _      = True
    LSComment == LSComment          = True
    _ == _                          = False

symbols = "()'"
whitespaces = "\n\t "
escapes = zip "ntr" "\n\t\r"

lexicalAnalyzer :: String -> [Lexeme]
lexicalAnalyzer [] = []
lexicalAnalyzer s = lexicalAnalyzer' LSWhitespace s

lexicalAnalyzer' :: LState -> String -> [Lexeme]
lexicalAnalyzer' s [] = maybeToList $ maybeLexeme s LSEOF
lexicalAnalyzer' s (c:cs) =
    let ns = lexicalSM c s
    in case maybeLexeme s ns of
        Just LError -> [ LError ]
        Just lexem -> lexem:(lexicalAnalyzer' ns cs)
        Nothing -> lexicalAnalyzer' ns cs

maybeLexeme :: LState -> LState -> Maybe Lexeme
maybeLexeme state nextState =
    case state of
      LSSymbol c -> Just $ lexemFromSymbol c
      LSError -> Just LError
      _ ->
        if state == nextState then Nothing
        else case state of
            LSWord w -> if isValidStateAfterWord nextState
                        then Just $ lexemFromWord $ reverse w
                        else Just LError
            LSString s -> Just $ LString $ reverse s
            _ -> Nothing

isValidStateAfterWord :: LState -> Bool
isValidStateAfterWord s =
    case s of
      (LSSymbol _) -> True
      LSWhitespace -> True
      LSComment -> True
      LSEOF -> True
      _ -> False

lexemFromWord :: String -> Lexeme
lexemFromWord w =
    case reads w :: [(Integer, String)] of
        [(i, "")] -> LInt i
        _ -> case reads w :: [(Float, String)] of
                [(f, "")] -> LFloat f
                _ -> LSymbol w

lexemFromSymbol :: Char -> Lexeme
lexemFromSymbol c = case c of
    '(' -> LSP
    ')' -> LCP
    '\'' -> LQuote
    _ -> LError

------
--  Lexical state machine
------

lexicalSM :: Char -> LState -> LState

lexicalSM c LSWhitespace | c `elem` whitespaces = LSWhitespace
lexicalSM c LSWhitespace | c `elem` symbols     = LSSymbol c
lexicalSM c LSWhitespace | c == '"'             = LSInString "" False
lexicalSM c LSWhitespace | c == ';'             = LSComment
lexicalSM c LSWhitespace                        = LSWord [c]

lexicalSM c (LSSymbol _) | c `elem` whitespaces     = LSWhitespace
lexicalSM c (LSSymbol _) | c `elem` symbols         = LSSymbol c
lexicalSM c (LSSymbol _) | c == '"'                 = LSInString "" False
lexicalSM c (LSSymbol _) | c == ';'                 = LSComment
lexicalSM c (LSSymbol _)                            = LSWord [c]

lexicalSM c (LSWord _) | c `elem` whitespaces       = LSWhitespace
lexicalSM c (LSWord _) | c `elem` symbols           = LSSymbol c
lexicalSM c (LSWord _) | c == '"'                   = LSError
lexicalSM c (LSWord _) | c == ';'                   = LSComment
lexicalSM c (LSWord w)                              = LSWord (c:w)

lexicalSM c     (LSInString s True) | isJust ec  = LSInString ((fromJust ec):s) False
    where ec = lookup c escapes
lexicalSM c     (LSInString s True)         = LSInString (c:s) False
lexicalSM '\\'  (LSInString s False)        = LSInString s True
lexicalSM '"'   (LSInString s False)        = LSString s
lexicalSM c     (LSInString s False)        = LSInString (c:s) False

lexicalSM c (LSString _) | c `elem` whitespaces     = LSWhitespace
lexicalSM c (LSString _) | c `elem` symbols         = LSSymbol c
lexicalSM c (LSString _) | c == ';'                 = LSComment
lexicalSM c (LSString _)                            = LSError

lexicalSM c (LSComment) | c == '\n' = LSWhitespace
lexicalSM c (LSComment)             = LSComment

lexicalSM c LSEOF           = error "No step behind EOF"

