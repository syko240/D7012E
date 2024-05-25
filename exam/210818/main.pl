% isSorted is a helper that verifies that a list is sorted
isSorted([]).
isSorted([_]).
isSorted([A,B|T]) :-
    A =< B,
    isSorted([B|T]).

sums(Denom, Limit, Coins) :-
    sums(Denom, [], Coins, Limit, 0).
sums(_, Coins, Coins, Limit, Limit) :-
    isSorted(Coins).
sums(L, Partial, Coins, Limit, Sum) :-
    Sum < Limit,
    member(X,L),
    S is Sum + X,
    sums(L, [X|Partial], Coins, Limit, S).

% main procedure
exchange1(Piles, S, Ways) :-
    findall(C, (sums(Piles,S,C)), L), length(L,Ways).