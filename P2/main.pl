% Compute Sublists

sum([], 0).
sum([H|T], Sum) :-
    sum(T, Rest),
    Sum is H + Rest.

sublist(List, Start, End, Sublist) :-
    Length is End - Start + 1,
    drop(List, Start, Dropped),
    take(Dropped, Length, Sublist).

drop(List, N, Result) :-
    length(Drop, N),
    append(Drop, Result, List).

take(List, N, Result) :-
    length(Result, N),
    append(Result, _, List).

findSets(List, Sets) :-
    length(List, Length),
    findall((Sum, I, J), (
        between(0, Length, I),
        between(I, Length, J),
        sublist(List, I, J, Sublist),
        sum(Sublist, Sum)
        ), Sets).

smallestKSets(K, List, Result) :-
    findSets(List, AllSets),
    length(AllSets, TotalSets),
    (TotalSets =:= 0 -> throw(error('list is empty'))
    ; K > TotalSets -> Result = AllSets
    ; take(AllSets, K, Result)
    ).

% smallestKSets(3, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3], Result).