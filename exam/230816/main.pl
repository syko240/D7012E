branch(a,b).
branch(a,c).
branch(b,d).
branch(b,e).
branch(b,f).
branch(c,g).
branch(f,h).
branch(f,i).

lca(A, A, A) :- !.
lca(A,B,R) :-
    (branch(X,A); branch(Y,B)),
    lca(X, Y, R), !.

sturdyLayer(PreviousLayer, Pile, NextLayer) :-
    findall(X, layer(PreviousLayer, Pile, 0, X), NextLayer).

layer(_, _, 14, []).
layer(Prev, [Brick|Pile], Acc, [Brick|NextLayer]) :-
    Acc2 is Acc + Brick,
    \+ member(Acc2, Prev) ; Acc2 = 14,
    layer(Prev, Pile, Acc2, NextLayer).