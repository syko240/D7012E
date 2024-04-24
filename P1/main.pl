% André Roaas

% move rooms
move(state(r1, Items, R1, R2, R3), state(r2, Items, R1, R2, R3), move_r2) :-
    member(steel_key, Items).

move(state(r2, Items, R1, R2, R3), state(r1, Items, R1, R2, R3), move_r1) :-
    member(steel_key, Items).

move(state(r1, Items, R1, R2, R3), state(r3, Items, R1, R2, R3), move_r3) :-
    member(brass_key, Items).

move(state(r3, Items, R1, R2, R3), state(r1, Items, R1, R2, R3), move_r1) :-
    member(brass_key, Items).

% pickup
move(state(Room, Items, R1, R2, R3), state(Room, NewItems, NewR1, NewR2, NewR3), pickUp(Item)) :-
    length(Items, Length),
    Length < 2,
    pickUp(Room, Item, Items, NewItems, R1, R2, R3, NewR1, NewR2, NewR3).

% drop
move(state(Room, Items, R1, R2, R3), state(Room, NewItems, NewR1, NewR2, NewR3), drop(Item)) :-
    remove(Item, Items, NewItems),
    drop(Room, Item, R1, R2, R3, NewR1, NewR2, NewR3).

pickUp(r1, Item, Items, [Item|Items], R1, R2, R3, NewR1, R2, R3) :-
    remove(Item, R1, NewR1).
pickUp(r2, Item, Items, [Item|Items], R1, R2, R3, R1, NewR2, R3) :-
    remove(Item, R2, NewR2).
pickUp(r3, Item, Items, [Item|Items], R1, R2, R3, R1, R2, NewR3) :-
    remove(Item, R3, NewR3).

drop(r1, Item, R1, R2, R3, [Item|R1], R2, R3).
drop(r2, Item, R1, R2, R3, R1, [Item|R2], R3).
drop(r3, Item, R1, R2, R3, R1, R2, [Item|R3]).

remove(Item, [Item|Old], Old).
remove(Item, [H|Old], [H|New]) :-
        remove(Item, Old, New).

initState(state(r1, [], [steel_key], [brass_key], [package])).
goal(state(r2, _, _, [package], _)).

% State(current room, picked items, r1 items, r2 items, r3 items)
solveR(State, 0, []) :-
    goal(State), !.

%set_prolog_flag(answer_write_options, [max_depth(0)]).

solveR(State, N, [(Action, NextState)|Trace]) :-
    N > 0,
    move(State, NextState, Action),
    M is N - 1,
    solveR(NextState, M, Trace).

% initState(S), solveR(S, 12, Trace).
% set_prolog_flag(answer_write_options, [max_depth(0)]), initState(S), solveR(S, 12, Trace).

output([]).
output([(A, S)|T]) :-
    format("~w~t~20|->~t~25|~w~n", [A, S]),
    output(T).

solve(N) :-
    initState(S),
    format("~w~n", S),
    solveR(S, N, Trace),
    output(Trace).