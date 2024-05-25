append2([], L2, L2).
append2([H|L1], L2, [H|L3]) :-
    append2(L1, L2, L3).

concat2([L], L).
concat2([H1,H2|LL], L) :-
    append2(H1, H2, R),
    concat2([R|LL], L).

% X = [],
% Y = [a,b,c,d] ;
% X = [a],
% Y = [b,c,d] ;
% X = [a,b],
% Y = [c,d] ;
% X = [a,b,c],
% Y = [d] ;
% X = [a,b,c,d],
% Y = [] ;
% false.

pack(AvailableBags, SuitcaseVolume, BagsPacked) :-
    findall(X, packing(AvailableBags, SuitcaseVolume, 0, X), BagsPacked).
    %sumList(Packs, 0, BagsPacked).

packing([], _, _, []) :- !.
packing([Bag|Bags], Vol, C, [Bag|Pack]) :-
    C1 is Bag + C,
    C1 =< Vol,
    packing(Bags, Vol, C1, Pack).
packing([_|Bags], Vol, C, Pack) :-
    packing(Bags, Vol, C, Pack).

sumList([], SumList, SumList).
sumList([H|Pack], Temp, SumList) :-
    sum(H, 0, Sum),
    sumList(Pack, [Sum|Temp], SumList).

sum([], Sum, Sum).
sum([H|L], Temp, Sum) :-
    New is Temp + H,
    sum(L, New, Sum).