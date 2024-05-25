% a and b and c
% (a and b) or c
% (a and b) or (not a and c)
% c or (a and b)
% c or (not c and a and b)
% (a and b) or (not (a and b) and c)

member2(A, [A|_]) :- true.
member2(_, []) :- fail.
member2(A, [_|L]) :-
    member2(A, L).

cashExchange(ValueList, Sum, ResultList) :-
    findall(X, cash(ValueList, Sum, 0, X), R),
    findbest(R, ResultList).

findbest([Best], Best).
findbest([R1,R2|R], Best) :-
    length(R1, L1),
    length(R2, L2),
    (L1 < L2 -> findbest([R1|R], Best)
    ; findbest([R2|R], Best)).

cash([], Sum, Sum, []).
cash([Val|Vals], Sum, Acc, [Val|R]) :-
    Acc2 is Acc + Val,
    Acc2 =< Sum,
    cash([Val|Vals], Sum, Acc2, R).
cash([_|Vals], Sum, Acc, R) :-
    cash(Vals, Sum, Acc, R).