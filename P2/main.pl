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

findSets(List, N, Sets) :-
    findall((Sum, I, J), (
        between(0, N, I),
        between(I, N, J),
        sublist(List, I, J, Sublist),
        sum(Sublist, Sum)
        ), Sets).

smallestKSets(K, List, Result) :-
    length(List, N),
    (N =:= 0 -> throw(error('list is empty'))
    ; findSets(List, N, AllSets),
    mergeSort(AllSets, SortedSets),
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

testCase(15, [-1, 2, -3, 4, -5, 6, -7, 8, -9, 10, -11, 12, -13,
    14, -15, 16, -17, 18, -19, 20, -21, 22, -23, 24, -25, 26,
    -27, 28, -29, 30, -31, 32, -33, 34, -35, 36, -37, 38, -39,
    40, -41, 42, -43, 44, -45, 46, -47, 48, -49, 50, -51, 52,
    -53, 54, -55, 56, -57, 58, -59, 60, -61, 62, -63, 64, -65,
    66, -67, 68, -69, 70, -71, 72, -73, 74, -75, 76, -77, 78,
    -79, 80, -81, 82, -83, 84, -85, 86, -87, 88, -89, 90, -91,
    92, -93, 94, -95, 96, -97, 98, -99, 100]).
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