reverse(L, R) :-
    rev(L, [], R2),
    R = R2.

rev([], R, R).
rev([H|L], Temp, R) :-
    rev(L, [H|Temp], R).

subset([], _).
subset([H|A], B) :-
    member(H, B),
    subset(A, B).

equal(A, B) :-
    subset(A, B),
    subset(B, A).

intersection([], _, []).
intersection([H|A], B, [H|U]) :-
    member(H, B),
    intersection(A, B, U).
intersection([_|A], B, U) :- intersection(A, B, U).

union([], [], []).
union([H|A], B, [H|U]) :-
    union(A, B, U).
union([], [H|B], [H|U]) :-
    \+member(H, U),
    union([], B, U).