-- ---------------------
-- Program: while.hs
-- Author: Faldict

-- On this homework, I worked 10 hours independently.
-- ----------------------
import Data.Map
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Name = String 
data Expr = Var Name 
          | BoolLit Bool
          | Lit Integer 
          | Neg Expr
          | Not Expr 
          | NotNot Expr
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
evalB e (BoolLit b) = b
evalB e (Not b) = not (evalB e b)
evalB e (NotNot b) = (evalB e b)
evalB e (Eq a b) = evalI e a == evalI e b
evalB e (Less a b) = evalI e a < evalI e b
evalB e (Leq a b) = evalI e a <= evalI e b
evalB e (Greater a b) = evalI e a > evalI e b
evalB e (Geq a b) = evalI e a >= evalI e b
evalB e (And a b) = evalB e a && evalB e b
evalB e (Or a b) = evalB e a || evalB e b 

evalI :: Store -> Expr -> Integer
evalI e (Lit x) = x
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

type Parser = Parsec Void String

_cond :: Parser Expr
_cond = do
  symbol "if" 
  c <- bexprparser
  symbol "then"
  e1 <- braces stmtSeq <|> stmtparser
  symbol "else"
  e2 <- braces stmtSeq <|> stmtparser
  return (IfThenElse c e1 e2)

_loop :: Parser Expr
_loop = do
  symbol "while"
  e1 <- bexprparser
  symbol "do"
  e2 <- braces stmtSeq <|> stmtparser
  return (WhileDo e1 e2)

_assign :: Parser Expr
_assign = do
  x <- identifier
  symbol ":="
  e <- aexprparser
  return (Assign x e)

_skip :: Parser Expr
_skip = do
  symbol "skip"
  return Skip 

stmtparser :: Parser Expr
stmtparser = (choice
  [ _skip <?> "skip"
  , _cond <?> "conditional statement"
  , _loop <?> "while loop"
  , _assign <?> "assign"
  ]) <?> "expr"

aexprparser = makeExprParser aexpr atable <?> "expression"
bexprparser = makeExprParser bexpr btable <?> "expression"

aexpr :: Parser Expr
aexpr = try (parens aexprparser)
  <|> try (Var <$> identifier)
  <|> try (Lit <$> integer)
  <|> try aexprparser
  <?> "arith expression"

bexpr :: Parser Expr
bexpr = try (parens bexprparser)
  <|> try (BoolLit True <$ symbol "true")
  <|> try (BoolLit False <$ symbol "false")
  <|> try rExpr
  <?> "bool expression"

rExpr :: Parser Expr
rExpr = do
  a1 <- aexprparser
  op <- (symbol ">" *> pure Greater) <|> (symbol "=" *> pure Eq) <|> (symbol "<" *> pure Less) <|> (symbol "<=" *> pure Leq) <|> (symbol ">=" *> pure Geq)
  a2 <- aexprparser
  return (op a1 a2)

sc :: Parser ()
sc = Lex.space space1 Text.Megaparsec.empty (Lex.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

integer :: Parser Integer
integer = lexeme Lex.decimal

symbol :: String -> Parser String
symbol = Lex.symbol sc

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
semicolon = symbol ";"

atable :: [[Operator Parser Expr]]
atable =
  [ [ prefix "-" Neg]
  , [ binaryL "*" Mul]
  , [ binaryL "+" Add , binaryL "-" Sub ]
  ]
  where
    prefix  name f = Prefix  (f <$ symbol name)
    binaryL name f = InfixL (f <$ symbol name)
    binaryR name f = InfixR (f <$ symbol name)

btable :: [[Operator Parser Expr]]
btable =
  [ [ prefix "¬" Not]
  , [ prefix "¬¬" NotNot]
  , [ binaryL "∧" And ]
  , [ binaryL "∨" Or ]
  ]
  where
    prefix  name f = Prefix  (f <$ symbol name)
    binary name f = InfixN (f <$ symbol name)
    binaryL name f = InfixL (f <$ symbol name)

reservedWords :: [String]
reservedWords = ["true", "false", "skip", "if", "then", "else", "while", "do"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

whileparser :: Parser Expr
whileparser = sc *> stmtSeq <* eof

stmt :: Parser Expr
stmt = braces stmtSeq <|> stmtparser

stmtSeq :: Parser Expr
stmtSeq = f <$> sepBy1 stmt semicolon
  -- if there's only one stmt return it without using ‘Seq’
  where f l = if length l == 1 then head l else Seq l

main :: IO ()
main = do
        code <- getContents
        case parse whileparser "" code of
             { Left err -> print err
             ; Right expr -> putStrLn $ showStore $ eval Map.empty expr
             }