mergeUnq(L1, L2, L) :-
    mySort(L1, L11),
    mySort(L2, L22),
    merge(L11, L22, L).

mySort([], []).
mySort([H], [H|L]) :-
    mySort([], L).
mySort([H,H1|L1], [H|L]) :-
    H < H1,
    mySort([H1|L1], L).
mySort([H,H1|L1], [H1|L]) :-
    H > H1,
    mySort([H|L1], L).
mySort([H,H1|L1], [H|L]) :-
    H = H1,
    mySort(L1, L).

merge([], [], []).
merge([], L2, L2).
merge(L1, [], L1).
merge([H|L1], [H1|L2], [H|L]) :-
    H < H1,
    merge(L1, [H1|L2], L).
merge([H|L1], [H1|L2], [H1|L]) :-
    H > H1,
    merge([H|L1], L2, L).
merge([H|L1], [H1|L2], [H|L]) :-
    H = H1,
    merge(L1, L2, L).


swap(t(A,B), t(C,D)) :-
    A = D,
    B = C.

swapAll([], []).
swapAll([H|L1], [H1|L2]) :-
    swap(H, H1),
    swapAll(L1, L2).