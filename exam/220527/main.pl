inBoth(X, L1, L2) :-
    member(X, L1),
    member(X, L2).

intersection(L1, L2, Intersection) :-
    findall(X, inBoth(X, L1, L2), Intersection).


eval(Env, plus(A,B), V) :- eval(Env, A, V1), eval(Env, B, V2), V is V1 + V2.
eval(Env, times(A,B), V) :- eval(Env, A, V1), eval(Env, B, V2), V is V1 * V2.
eval(Env, X, Val) :- getVal(X, Env, Val).
eval(_, X, X) :- integer(X).

getVal(X, [(X, Val)| _], Val).
getVal(X, [_|T], Val) :- getVal(X, T, Val).