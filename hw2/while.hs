import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- cite code from Haskell wiki
type Name = String 
data Expr = Var Name 
          | Con Bool
          | Nv Integer 
          | Not Expr 
          | Eq Expr Expr
          | Less Expr Expr
          | Leq Expr Expr
          | Greater Expr Expr
          | Geq Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Assign Name Expr
          | IfThenElse Expr Expr Expr
          | WhileDo Expr Expr
          | Skip
          | Seq [Expr]
    deriving Show

-- data Stmt = Skip | Name := Expr | If Expr Stmt Stmt | While Expr Stmt
--           | Seq [Stmt]
--     deriving Show
-- lexeme :: Parser a -> Parser a 
-- lexeme p = p <* whitespace

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=:+-*<>¬∧"
              , opLetter = oneOf "=:<>+-*¬∧"
              , reservedOpNames = ["∧", "=", ":=", "+", "-", "*", "<", "<=", ">", ">=", "¬"]
              , reservedNames = ["true", "false", "skip",
                                 "if", "then", "else",
                                 "while", "do"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [ Prefix (m_reservedOp "¬" >> return (Not))]
        , [ binaryL "*" (Mul) ]
        , [ binaryL "+" (Add)
          , binaryL "-" (Sub)]
        , [ binaryN "<" (Less)
          , binaryN "<=" (Leq)
          , binaryN "=" (Eq)
          , binaryN ">" (Greater)
          , binaryN ">=" (Geq)
          ]
        , [ binaryR "∧" (And) ]
        , [ binaryR "∨" (Or) ]
        ]
        where
          binaryN op f = Infix (m_reservedOp op >> spaces >> return f) AssocNone
          binaryL op f = Infix (m_reservedOp op >> spaces >> return f) AssocLeft
          binaryR op f = Infix (m_reservedOp op >> spaces >> return f) AssocRight

term = m_parens exprparser
       <|> (fmap Var m_identifier)
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))
       <|> do { s <- string "-" <|> return []
              ; cs <- many1 digit
              ; spaces
              ; return $ Nv $ read (s ++ cs)
              }

whileparser :: Parser Expr
whileparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Expr
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "skip" >> return Skip)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (Assign v e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; return (IfThenElse b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; return (WhileDo b p)
                     }
              <|> try exprparser

main :: IO ()
main = do
    a <- getLine
    case parse whileparser "" a of
             { Left err -> print err
             ; Right ans -> print ans
             }