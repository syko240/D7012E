larger(_, []) :- true.
larger(X, [H|List]) :-
    H < X,
    larger(X, List).

removeAll(X, List, Result) :-
    rem2(X, List, [], Result).

rem2(_, [], Result, Result).
rem2(X, [H|List], Temp, Result) :-
    (X == H -> rem2(X, List, Temp, Result)
    ; rem2(X, List, [H|Temp], Result)).

isA(h,e).
isA(i,e).
isA(j,f).
isA(k,g).
isA(l,c).
isA(l,d).
isA(e,a).
isA(f,a).
isA(f,b).
isA(g,c).
isA(d,b).
isA(p,h).

same(X, Y) :-
    isA(Y, A),
    findall(Z, (isA(A, B); isA(B, A)), C),
    member(X, C).