-- André Roaas

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- 1
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> (\(_, b) -> b)

-- 1
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> (\(a, _) -> a)

-- 1
spaces :: Parser String
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- 1
letter :: Parser Char
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- 1
chars :: Int -> Parser String
chars n
    | n <= 0 = return ""
    | otherwise = char # chars (n-1) >-> \(c, cs) -> c : cs

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- 1
require :: String -> Parser String
require w  = accept w ! err ("Expected " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')