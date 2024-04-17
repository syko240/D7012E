sublist(List, Start, End, Sublist) :-
    findall(Elem, (nth1(Index, List, Elem), Index >= Start, Index =< End), Sublist).

findSets(List, Sets) :-
    length(List, Length),
    findall((Sublist, I, J), (between(1, Length, I), between(I, Length, J), sublist(List, I, J, Sublist)), Sets).

smallestKSets(K, List, Result) :-
    findSets(List, AllSets),
    length(AllSets, TotalSets),
    (TotalSets =:= 0 -> throw(error('list is empty'))
    ; K > TotalSets -> Result = AllSets
    ; length(Result, K), append(Result, _, AllSets)
    ).

% smallestKSets(8, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3], Result).