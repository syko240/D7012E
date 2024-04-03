-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson
-- André Roaas
import Data.Char

-- Already supports function application through the 'App String EXPR' constructor
data EXPR = Const Int
    | Var String
    | Op String EXPR EXPR
    | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
    where
        notfirst p (_, []) = True
        notfirst p (_, x : xs) = not (p x)

        buildnumber :: String -> (EXPR, String)
        buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
            where
                accdigits :: (EXPR, String) -> (EXPR, String)
                accdigits (Const n, y : ys) = (Const(10*n+(ord y - 48)), ys)

        buildvar :: String -> (EXPR, String)
        buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
            where
                accletters :: (EXPR, String) -> (EXPR, String)
                accletters (Var s, y : ys) = (Var (s ++[y]), ys)

        buildexpr :: String -> (EXPR, String)
        buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
            where
                accterms :: (EXPR, String) -> (EXPR, String)
                accterms (term, y : ys) = (Op (y : []) term term1, zs)
                    where
                        (term1, zs) = buildterm ys

        buildterm :: String -> (EXPR, String)
        buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
            where
                accfactors :: (EXPR, String) -> (EXPR, String)
                accfactors (fact, y : ys) = (Op (y : []) fact fact1, zs)
                    where
                        (fact1, zs) = buildfactor ys

        buildfactor :: String -> (EXPR, String)
        buildfactor [] = error "missing factor"
        buildfactor ('(' : xs) =  case buildexpr xs of (e, ')' : ws) -> (e, ws); _ -> error "missing factor"
        buildfactor (x : xs)
            | isDigit x = buildnumber (x : xs)
            | isLetter x = case buildvar (x : xs) of
                            (Var s, '(' : zs) -> let (e, ws)=buildfactor ('(' : zs) in (App s e, ws)
                            p -> p
            | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
-- unparse was missing pattern for handling 'App' constructor
unparse (App func e) = func ++ "(" ++ unparse e ++ ")"

eval :: EXPR -> [(String, Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-- We can use the already defined App from EXPR
eval (App "sin" e) env = sin (eval e env)
eval (App "cos" e) env = cos (eval e env)
eval (App "log" e) env = log (eval e env)
eval (App "exp" e) env = exp (eval e env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
    | id == id2 = Const 1
    | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
    Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
    Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)

-- diff for sin, cos, log and exp
-- d(sin(u))/dx = cos(u) * du/dx
diff v (App "sin" e) = Op "*" (App "cos" e) (diff v e)
-- d(cos(u))/dx = -sin(u) * du/dx
diff v (App "cos" e) = Op "*" (Op "-" (Const 0) (App "sin" e)) (diff v e)
-- d(log(u))/dx = 1/u * du/dx
diff v (App "log" e) = Op "*" (Op "/" (Const 1) e) (diff v e)
-- d(exp(u))/dx = exp(u) * du/dx
diff v (App "exp" e) = Op "*" (App "exp" e) (diff v e)

diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
    let (lefts, rights) = (simplify left, simplify right) in
        case (oper, lefts, rights) of
            ("+", e, Const 0) -> e
            ("+", Const 0, e) -> e
            ("*", e, Const 0) -> Const 0
            ("*", Const 0, e) -> Const 0
            ("*", e, Const 1) -> e
            ("*", Const 1, e) -> e
            ("-", e, Const 0) -> e
            ("/", e, Const 1) -> e
            ("-", le, re)     -> if left==right then Const 0 else Op "-" le re
            (op, le, re)      -> Op op le re

-- Simplify the argument of the function
simplify (App func e) = App func (simplify e)

-- The 'mkfun' function will create a function that takes a float, representing the value
-- of the variable var, and returns a float by evaluating the 'EXPR' body where var is
-- bound to the input float value
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, Var var) = \x -> eval body [(var, x)]

-- example
f = mkfun (parse "x*x+2", Var "x")

-- Newton-Raphson formula: x_{n+1} = x_n - (f x_n / f' x_n)
findzero :: String -> String -> Float -> Float
findzero name body x0 = let
    expr = parse body
    expr' = diff (Var name) expr
    f = mkfun (expr, Var name)
    f' = mkfun (expr', Var name)
    newtonRaphson x = x - (f x / f' x)
    iterate x = let next = newtonRaphson x
        in if abs (x - next) < 0.0001 then next else iterate next
    in iterate x0

-- findzero "x" "x*x*x+x-1" 1.0
-- = 0.68232775
-- findzero "y" "cos(y)*sin(y)" 2.0
-- = 1.5707964