import qualified Data.Map as Map

type VarName = String
type Value = Integer
type Store = Map VarName Value

data Expr = Lit Int
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Var VarName
        | Assign Var Expr
        | BoolLit Bool
        | Not Expr
        | Eq Expr Expr
        | Neq Expr Expr
        | Less Expr Expr
        | Leq Expr Expr
        | Greater Expr Expr
        | Geq Expr Expr
        | IfThenElse Expr Expr Expr
        | WhileDo Expr Expr
        deriving (Show, Eq)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a 
lexeme p = p <* whitespace

numberParser :: Parser Expr
numberParser = lexeme $ do
    s <- string "-" <|> return []
    cs <- many1 digit
    return $ Lit $ read (s ++ cs)

mulParser :: Parser Expr
mulParser = chainl1 numberParser op
    where op = Mul <$ lexeme (char '*')

arithParser :: Parser Expr
arithParser = chainl1 mulParser op
    where op = Add <$ lexeme (char '+')
               <|> Sub <$ lexeme (char '-') 
