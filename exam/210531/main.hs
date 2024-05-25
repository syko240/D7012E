stars :: [[Char]]
stars = ["*"] ++ map (++"*") stars

data Symbol = Num Int | Mult | Add | Sub 

rpn :: [Symbol] -> Int
rpn s = rpn2 s []
    where
        rpn2 [] [s] = s
        rpn2 (Num x : stack) cock = rpn2 stack (x : cock)
        rpn2 (Mult : stack) (x1 : x2 : cock) = rpn2 stack ((x1*x2) : cock)
        rpn2 (Add : stack) (x1 : x2 : cock) = rpn2 stack ((x1+x2) : cock)
        rpn2 (Sub : stack) (x1 : x2 : cock) = rpn2 stack ((x2-x1) : cock)