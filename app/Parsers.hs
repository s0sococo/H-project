module Parsers where

-- apply expr "-(3.8 + 1) + 2.5 * (-10 + 4) "
-- (-19.8)

import Data.Char (isSpace, isDigit)
import Control.Applicative
import Control.Monad (liftM, ap)

newtype Parser a = Parser (String -> [(a, String)])


parse :: Parser a -> String -> [(a, String)]
parse (Parser p) str = p str

-- Monad
instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser (\cs -> [(a, cs)])
    (<*>) = ap

instance Monad Parser where
    p >>= f = Parser (\cs -> concat [parse (f a) cs' |
                                   (a, cs') <- parse p cs])

-- MonadPlus
instance Alternative Parser where
    empty = Parser (\cs -> [])
    p <|> q = Parser (\cs -> case parse p cs of
                           []        -> parse q cs
                           ps -> ps)

-- deterministic choice operator
(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = Parser (\cs -> case parse (p <|> q) cs of
                               []     -> []
                               (x:_) -> [x])

-- left-associative
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (chainl1 p op) <!> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
    where rest a = (do 
                        f <- op
                        b <- p
                        rest (f a b))
                    <!> return a

chainl2 :: Parser a -> Parser b -> Parser (a -> b -> a) -> Parser a
chainl2 p1 p2 op = do {a <- p1; rest a}
    where rest a = (do 
                        f <- op
                        b <- p2
                        rest (f a b))
                    <!> return a

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs)-> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

symb :: String -> Parser String
symb cs = token $ string cs

num :: Parser Double
num = do part1 <- some digit
         part2 <- (char '.' >> some digit) <!> return "0"
         return (fromIntegral (read part1) + read part2 / (10 ^ length part2))

number :: Parser Double
number = token num

expr = chainl2 term term2 addop
term = chainl2 factor factor2 mulop
term2 = chainl1 factor2 mulop
factor = factor2 <!> do {symb "-"; n <- factor2; return (-n)}
factor2 = number <!> do {symb "("; n <- expr; symb ")"; return n}

addop = do {symb "+"; return (+)} <!> do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)}

apply :: Parser a -> String -> [(a, String)]
apply p = parse $ do {space; p}