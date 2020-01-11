-- parser
import Data.Char
import Control.Monad
import Control.Applicative ((<|>))
import Text.Parsec (parse, many, many1, oneOf, string, char, digit, chainl1)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char  (oneOf, char, digit, satisfy)
import qualified Text.Parsec.Combinator (many1, chainl1)
import Text.Parsec.Error (ParseError)

data Expr = Nv Int
          | Add Expr Expr
          | Mul Expr Expr
     --   | Sub Expr Expr
    deriving Show

eval :: Expr -> Int
eval ex = case ex of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Nv x -> x 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a 
lexeme p = p <* whitespace

numberParser :: Parser Expr
numberParser = lexeme $ do
    s <- string "-" <|> return []
    cs <- many1 digit
    return $ Nv $ read (s ++ cs)

mulParser :: Parser Expr
mulParser = chainl1 numberParser op
    where op = Mul <$ lexeme (char '*')

addParser :: Parser Expr
addParser = chainl1 mulParser op
    where op = Add <$ lexeme (char '+')

-- subParser :: Parser Expr
-- subParser = chainl1 mulParser op 
--     where op = Sub <$ lexeme (char '-')

run :: String -> Either ParseError Int
run s = 
    case parse addParser "<stdin>" s of
        Left err -> Left err
        Right e -> Right (eval e)

main :: IO ()
main = do
    a <- getLine
    print . either show show $ run a