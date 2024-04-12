module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
-- 3.a
data Statement = Assignment String Expr.T
    | If Expr.T Statement Statement
    | Skip
    | Read String
    | Write Expr.T
    | Begin [Statement]
    | While Expr.T Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- 3.b
if' = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

skip' = (accept "skip" #- require ";") >-> buildSkip
buildSkip _ = Skip

read' = (accept "read" -# word #- require ";") >-> buildRead
buildRead = Read

write' = (accept "write" -# Expr.parse #- require ";") >-> buildWrite
buildWrite = Write

begin = (accept "begin" -# iter parse #- require "end") >-> buildBegin
buildBegin = Begin

while = (accept "while" -# Expr.parse) # (require "do" -# parse #- require ";") >-> buildWhile
buildWhile (e, s) = While e s

-- 3.d
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Assignment word stmts) dict input =

instance Parse Statement where
    -- 3.c
    parse = assignment ! if' ! skip' ! read' ! write' ! begin ! while
    toString = error "Statement.toString not implemented"