doubles([]).
doubles([H1,H2|L]) :-
    H1 = H2,
    doubles(L).

repeats([], _).
repeats([Elem|Y], N) :-
    check(Y, N, Elem, Y1),
    repeats(Y1, N).

check(Y, 1, _, Y).
check([H1|Y], N, Elem, Y1) :-
    H1 = Elem,
    M is N - 1,
    check(Y, M, Elem, Y1).

% (A and B and C) or (A and not B and C)

edge(a,b).
edge(b,c).
edge(c,e).
edge(a,e).
edge(e,d).
edge(d,c).
edge(f,d).
edge(f,e).