del_nth(List, N, R) :-
    N >= 1,
    h(List, N, N, R).

h([], _, _, []).
h([_|T], 1, N, R) :- h(T, N, N, R), !.
h([H|T], C, N, [H|R]) :-
    M is C - 1,
    h(T, M, N, R).


arc(d, a).
arc(c, g).
arc(e, c).
arc(d, b).
arc(f, c).
arc(f, e).
arc(d, g).
arc(c, e).
arc(c, d).

exists(N, X) :-
    (arc(X, _);arc(_, X)),
    out(X, N1),
    in(X, N2),
    M is N1 + N2,
    M >= N, !.

out(Node, N) :-
    findall(Node, arc(Node, _), X),
    length(X, N).

in(Node, N) :-
    findall(Node, arc(_, Node), X),
    length(X, N).
