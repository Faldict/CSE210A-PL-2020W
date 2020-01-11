-- parser
import Data.Char
import Control.Monad
import Control.Applicative
import Text.Parsec

type nv = Int

data Term = nv
          | Mul Term Term
          deriving Show

data Expr = Term
          | Add Expr Expr
          | Sub Expr Expr
    deriving Show

numberParser :: Parsec String st nv
numberParser = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

main :: IO ()
main = do
    putStr "λ> "
    a <- getLine
    print $ eval $ run a