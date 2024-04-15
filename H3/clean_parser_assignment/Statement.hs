-- André Roaas

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
    | Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- 3.b
if' = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse >-> build
    where
        build ((e, s1), s2) = If e s1 s2

skip' = (accept "skip" #- require ";") >-> build
    where
        build _ = Skip

read' = (accept "read" -# word #- require ";") >-> build
    where
        build = Read

write' = (accept "write" -# Expr.parse #- require ";") >-> build
    where
        build = Write

begin = (accept "begin" -# iter parse #- require "end") >-> build
    where
        build = Begin

while = (accept "while" -# Expr.parse #- require "do") # parse >-> build
    where
        build (e, s) = While e s

repeat' = (accept "repeat" -# parse #- require "until") # (Expr.parse #- require ";") >-> build
    where
        build (s, e) = Repeat s e

-- 3.d
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec [] _ _ = []

exec (Assignment var expr : stmts) dict input =
    exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input

exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0 then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip : stmts) dict input =
    exec stmts dict input

exec (Read var : stmts) dict input =
    exec stmts (Dictionary.insert (var, head input) dict) (tail input)

exec (Write expr : stmts) dict input =
    Expr.value expr dict : exec stmts dict input

exec (Begin xs : stmts) dict input =
    exec (xs ++ stmts) dict input

exec (While cond s : stmts) dict input =
    if Expr.value cond dict == 0 then exec stmts dict input
    else exec (s : While cond s : stmts) dict input

exec (Repeat s cond : stmts) dict input =
    exec (s : If cond Skip (Repeat s cond) : stmts) dict input

instance Parse Statement where
    -- 3.c
    parse = assignment ! if' ! skip' ! read' ! write' ! begin ! while ! repeat'
    -- 5
    toString = stringBuilder 0

stringBuilder :: Int -> Statement -> String
stringBuilder indent stmt = case stmt of
    Assignment var expr -> indentString ++ var ++ ":=" ++ Expr.toString expr ++ ";\n"
    If cond thenStmt elseStmt -> indentString ++ "if " ++ Expr.toString cond ++ " then\n"
        ++ stringBuilder (indent + 2) thenStmt
        ++ indentString ++ "else\n"
        ++ stringBuilder (indent + 2) elseStmt
    Skip -> indentString ++ "skip;\n"
    Read var -> indentString ++ "read " ++ var ++ ";\n"
    Write expr -> indentString ++ "write " ++ Expr.toString expr ++ ";\n"
    Begin xs -> indentString ++ "begin\n"
        ++ concatMap (stringBuilder (indent + 2)) xs
        ++ indentString ++ "end\n"
    While cond s -> indentString ++ "while " ++ Expr.toString cond ++ " do\n"
        ++ stringBuilder (indent + 2) s
    Repeat s cond -> indentString ++ "repeat\n"
        ++ stringBuilder (indent + 2) s
        ++ indentString ++ "until " ++ Expr.toString cond ++ ";\n"
    where indentString = replicate indent ' '