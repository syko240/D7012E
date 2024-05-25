first([First|List], First).

last([Last], Last).
last([First|List], Last) :-
    last(List, Last).

%middle([], M) :- fail.
middle([M], M).
middle([M,_], M).
middle([X|L], M) :-
    reverse(L, [], R),
    middle(R, M).

reverse([], R, R).
reverse([Head|Tail], NewList, R) :-
    reverse(Tail, [Head|NewList], R).

del(X,[X|L],L).
del(X,[A|L],[A|L1]):- del(X,L,L1).
test(L, A) :- del(A, L, L2), del(B, L2, _), A = B.

test2(L,A) :- !, del(A,L,L2), del(B,L2,_), A = B.

exam(first).
exam(second).
exam(third).

genList(0,[]) :- !.
genList(N, [_ | L]) :- N1 is N-1, genList(N1,L).

leftDifferent([_]).
leftDifferent([X, Y | Xs]) :- 
    exam(X), 
    exam(Y), 
    X \= Y, 
    leftDifferent([Y|Xs]).

solution(N,List) :-
    genList(N,List),
    leftDifferent(List),
    first(List,F),
    last(List,L),
    F \= L.

allWays(N, LL) :-
    findall(List, solution(N, List), LL).

solution2(N,L,F,S,T) :-
    solution(N,L), % get solution without constraints
    %
    % check constraints:
    %
    findall(whatever, (member(first,L)), Fs),
    length(Fs, LenFs),
    LenFs =< F, % no more first used than what exists
    %
    findall(whatever, (member(second,L)), Ss),
    length(Ss, LenSs),
    LenSs =< S,
    %
    findall(whatever, (member(third,L)), Ts),
    length(Ts, LenTs),
    LenTs =< T.

allWays2(N,Ls,F,S,T) :- findall(L, solution2(N,L,F,S,T), Ls).
