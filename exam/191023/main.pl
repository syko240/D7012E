factorial(0, 1) :- !.
factorial(N, FN) :-
    M is N - 1,
    factorial(M, FN1),
    FN is N * FN1.

factorials(N, L) :- h(N, [], L).
h(N, Temp, L) :-
    N >= 0, 
    M is N - 1,
    h(M, [F|Temp], L),
    factorial(N, F), !.
h(_, L, L).

cashExchange(ValueList, Sum, ResultList) :-
    sort2(ValueList, R),
    cashExchangeHelper(R, Sum, [], ResultList).

cashExchangeHelper(_, 0, Acc, Acc) :- !.
cashExchangeHelper(Values, Sum, Acc, ResultList) :-
    member(Value, Values),
    Value =< Sum,
    NewSum is Sum - Value,
    cashExchangeHelper(Values, NewSum, [Value|Acc], ResultList).

insert(Elem, [], [Elem]).
insert(Elem, [H|T], [Elem,H|T]) :-
    Elem >= H.
insert(Elem, [H|T], [H|ResultTail]) :-
    Elem < H,
    insert(Elem, T, ResultTail).

sort2([], []).
sort2([H|T], SortedList) :-
    sort2(T, SortedTail),
    insert(H, SortedTail, SortedList).
