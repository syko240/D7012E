parts(L, [X1, X2]) :-
    append([X1, X2], L).

partsN(L, 1, [L]).
partsN(L, N, [F|R]) :-
    N > 1,
    append(F,Rest,L),
    N2 is N-1,
    partsN(Rest, N2, R).