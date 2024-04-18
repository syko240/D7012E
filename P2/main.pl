% André Roaas

% Sorting

merge([], Right, Right).
merge(Left, [], Left).
merge([(Sum1, I1, J1)|T1], [(Sum2, I2, J2)|T2], [(Sum1, I1, J1)|Merged]) :-
    Sum1 =< Sum2,
    merge(T1, [(Sum2, I2, J2)|T2], Merged).
merge([(Sum1, I1, J1)|T1], [(Sum2, I2, J2)|T2], [(Sum2, I2, J2)|Merged]) :-
    Sum1 > Sum2,
    merge([(Sum1, I1, J1)|T1], T2, Merged).

mergeSort([], []).
mergeSort([X], [X]).
mergeSort(List, Sorted) :-
    length(List, Len),
    Half is Len // 2,
    length(Left, Half),
    append(Left, Right, List),
    mergeSort(Left, SortedLeft),
    mergeSort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

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
    ; mergeSort(AllSets, SortedSets),
    take(SortedSets, K, Result)
    ).

% Output Formating

output(List, Results) :-
    format("\tEntire list: ~w~n~n", [List]),
    format("\tsize\ti\tj\tsublist~n"),
    printRows(Results, List).

printRows([], _).
printRows([(Sum, I, J) | T], List) :-
    I1 is I + 1, J1 is J + 1,
    sublist(List, I, J, Sublist),
    format("\t~d\t~d\t~d\t~w~n", [Sum, I1, J1, Sublist]),
    printRows(T, List).

% Testing

testCase(6, [24, -11, -34, 42, -24, 7, -19, 21]).
testCase(8, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]).

test :-
    forall(testCase(K, List),
    (smallestKSets(K, List, Result),
    output(List, Result),
    format("\n"))).

:- initialization main.
main :-
    test.

% smallestKSets(3, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3], Result).