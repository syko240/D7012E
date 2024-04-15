-- André Roaas

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
-- 4
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = parseProgram
  toString = stringBuilder

parseProgram = iter Statement.parse >-> build
  where
    build xs = Program xs

exec :: T -> [Integer] -> [Integer]
exec (Program p) xs = Statement.exec p Dictionary.empty xs

-- 5
stringBuilder :: T -> String
stringBuilder (Program xs) = concatMap Statement.toString xs