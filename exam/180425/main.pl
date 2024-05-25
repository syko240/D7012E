fib(0, 0).
fib(1, 1).
fib(N, FN) :-
    N > 1,
    N1 is N-1,
    N2 is N-2,
    fib(N1, F1),
    fib(N2, F2),
    FN is F2 + F1, !.

fibs(N, L) :-
    h(N, [], L), !.

h(N, Temp, L) :-
    M is N-1,
    fib(N, FN),
    h(M, [FN|Temp], L).
h(_, L, L).

fibs1(0, [0]).
fibs1(N, [L|FN]) :-
    M is N-1,
    fib(N, FN),
    fibs1(M, L), !.

arc(n1,n2).
arc(n2,n5).
arc(n2,n3).
arc(n3,n7).
arc(n3,n4).
arc(n6,n2).

path(B, B, []).
path(A, B, [arc(A, N)|P]) :-
    arc(A, N),
    path(N, B, P).

add(arc(A, B)) :-
    \+ path(B, A, _),
    asserta(arc(A, B)).

allPath(N, L) :-
    findall(P, (path(A, B, P), length(P, M), M is N-1), L).