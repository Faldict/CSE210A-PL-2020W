-- ---------------------
-- Program: while.hs
-- Author: Faldict

-- On this homework, I worked 8 hours independently.
-- ----------------------

import Data.Map
import qualified Data.Map as Map
import Control.Applicative((<*))
import Control.Monad.State
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- cite some parsec code from Haskell wiki
type Name = String 
data Expr = Var Name 
          | Con Bool
          | Nv Integer 
          | Neg Expr
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

type Store = Map Name Integer
showStore :: Store -> String
showStore e = case toList e of
    [] -> "{}"
    s -> "{" ++ (foldl1 showTwo (Prelude.map showKeyValue s)) ++ "}"
              where showKeyValue (k, a) = k ++ " → " ++ (show a)
                    showTwo a b = a ++ ", " ++ b

evalB :: Store -> Expr -> Bool
evalB e (Con b) = b
evalB e (Not b) = not (evalB e b)
evalB e (Eq a b) = evalI e a == evalI e b
evalB e (Less a b) = evalI e a < evalI e b
evalB e (Leq a b) = evalI e a <= evalI e b
evalB e (Greater a b) = evalI e a > evalI e b
evalB e (Geq a b) = evalI e a >= evalI e b
evalB e (And a b) = evalB e a && evalB e b
evalB e (Or a b) = evalB e a || evalB e b 

evalI :: Store -> Expr -> Integer
evalI e (Nv x) = x
evalI e (Neg x) = negate $ evalI e x
evalI e (Var x) = findWithDefault 0 x e
evalI e (Mul p q) = evalI e p * evalI e q
evalI e (Add p q) = evalI e p + evalI e q
evalI e (Sub p q) = evalI e p - evalI e q

eval :: Store -> Expr -> Store
eval e (Skip) = e
eval e (IfThenElse b p q) = case evalB e b of
                        True -> eval e p
                        False -> eval e q
eval e (WhileDo b p) = case evalB e b of
                   True -> eval e (Seq [p, WhileDo b p])
                   False -> e
eval e (Assign v a) = union e' e
                where e' = insert v (evalI e a) Map.empty
eval e (Seq []) = Map.empty
eval e (Seq (x:xs)) = union (eval e' (Seq (xs))) e'
                where e' = eval e x

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "=:+*<>¬∧"
              , opLetter = oneOf "=:<>+-*¬∧∨"
              , reservedOpNames = ["-", "∨", "∧", "=", ":=", "+", "*", "<", "<=", ">", ">=", "¬"]
              , reservedNames = ["true", "false", "skip",
                                 "if", "then", "else",
                                 "while", "do"]
              }

TokenParser{ parens = m_parens
           , braces = m_braces
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [ Prefix (m_reservedOp "¬" >> return (Not))]
        , [ Prefix (m_reservedOp "-" >> return (Neg))]
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
       <|> (m_reserved "true" >> spaces >> return (Con True))
       <|> (m_reserved "false" >> spaces >> return (Con False))
       <|> do { cs <- many1 digit
              ; spaces
              ; return $ Nv $ read cs
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
                     ; p <- stmtparser <|> m_braces stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser <|> m_braces stmtparser
                     ; return (IfThenElse b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser <|> m_braces stmtparser
                     ; return (WhileDo b p)
                     }
              <|> exprparser

getLines :: IO [String]
getLines = do
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      return (x:xs)

main :: IO ()
main = do
    -- codes <- getLines
    -- let code = foldl1 (\a b -> a ++ b) codes in
        code <- getLine
        case parse whileparser "" code of
             { Left err -> print err
             ; Right expr -> putStrLn $ showStore $ eval Map.empty expr
             }