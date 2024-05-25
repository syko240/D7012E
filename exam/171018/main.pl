select1(X, F, R) :-
    select2(X, F, R),
    F \= R.

select2(_, [], []).
select2(X,[H|F],[H|R]) :-
    X \= H,
    select2(X, F, R).
select2(X, [H|F], R) :-
    X = H,
    R = F, !.

pickTwoDifferent(L, E1, E2) :-
    select1(E1, L, L1),
    select1(E2, L1, _),
    E1 \= E2.

subLists(L, R) :-
    length(L, Len),
    findall(X, 
        (between(0, Len, I),
        between(I, Len, J),
        sublist(L, I, J, X)),
        R).

sublist(L, I, J, R) :-
    Len is J - I + 1,
    drop(L, I, R1),
    take(R1, Len, R).

take(_, 0, []).
take([H|L], I, [H|R]) :-
    I1 is I - 1,
    take(L, I1, R).

drop(L, 0, L).
drop([_|L], I, R) :-
    I1 is I - 1,
    drop(L, I1, R).

keep([], _, []).
keep([H|L], Max, [H|R]) :-
    max(H, 0, M),
    M =< Max,
    keep(L, Max, R).
keep([_|L], Max, R) :-
    keep(L, Max, R).

max([], Max, Max).
max([H|L], Temp, Max) :-
    New is H + Temp,
    max(L, New, Max).