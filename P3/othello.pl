/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: André Roaas
%    Student user id  : androa-0
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.], %(1,3)
	        [.,.,1,2,.,.], %(2,4)
	        [.,.,2,1,.,.], %(3,1)
            [.,.,.,.,.,.], %(4,2)
	        [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
initialize(InitialState, InitialPlyr) :- 
	initBoard(InitialState), 
	InitialPlyr = 1.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

% dont need to check for tie since its done in play.pl
count([], 0, 0).
count([Row|Rows], C1, C2) :-
    count(Rows, RestC1, RestC2),
    count_row(Row, RowC1, RowC2),
    C1 is RestC1 + RowC1,
    C2 is RestC2 + RowC2.

count_row([], 0, 0).
count_row([1|T], C1, C2) :- count_row(T, RestC1, C2), C1 is RestC1 + 1.
count_row([2|T], C1, C2) :- count_row(T, C1, RestC2), C2 is RestC2 + 1.
count_row([.|T], C1, C2) :- count_row(T, C1, C2).

winner(State, Plyr) :-
    terminal(State),
    count(State, C1, C2),
    (C1 < C2, Plyr = 1;
     C2 < C1, Plyr = 2).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
% Check if the game is a tie
tie(State) :-
    terminal(State),
    count(State, C1, C2),
    C1 == C2.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
% Check if the state is terminal
terminal(State) :-
    moves(1, State, MvList1),
    moves(2, State, MvList2),
    MvList1 = [],
    MvList2 = [].



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, Board, MvList) :-
    findall([X, Y], validmove(Plyr, Board, [X, Y]), Moves),
    sort(Moves, MvList).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
% Apply move and calculate the resulting board and the next player
nextState(Plyr, [X, Y], State, NewState, NextPlyr) :-
    set(State, TempState, [X, Y], Plyr),
    directions(Dir),
    flip(Plyr, [X, Y], Dir, TempState, NewState),
    opponent(Plyr, Opp),
    moves(Opp, NewState, MvList),
    (MvList == [] -> NextPlyr = Plyr ; NextPlyr = Opp).

flip(_, _, [], State, State).
flip(Plyr, [X, Y], [Dir|Dirs], State, NewState) :-
    %format('dir: ~w ~n', [Dir]),
    flip_in_direction(Plyr, [X, Y], Dir, State, State, TempState),
    flip(Plyr, [X, Y], Dirs, TempState, NewState).

flip_in_direction(Plyr, [X, Y], [DX, DY], S, State, NewState) :-
    NX is X + DX,
    NY is Y + DY,
    %format('pos: [~w,~w] ~n', [NX, NY]),
    (inside_board(NX, NY) -> get(State, [NX, NY], Val), opponent(Plyr, Opp),
        (Val == Opp ->  set(State, TempState, [NX, NY], Plyr),
            flip_in_direction(Plyr, [NX, NY], [DX, DY], S, TempState, NewState)
        ; Val == Plyr -> NewState = State
        ; Val == '.' -> NewState = S
        ; NewState = State)
    ; NewState = S).

directions([[0, 1], [1, 1], [1, 0], [1, -1], [0, -1], [-1, -1], [-1, 0], [-1, 1]]).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
% Check if a specific proposed move is valid for the player
opponent(1, 2).
opponent(2, 1).

validmove(Plyr, State, [X, Y]) :-
    get(State, [X, Y], Val),
    Val == '.',
    opponent(Plyr, Opp),
    directions(Dirs),
    check(Plyr, Opp, State, [X, Y], Dirs).

check(Plyr, Opp, State, [X, Y], [Dir|Dirs]) :-
    (check_direction(Plyr, Opp, State, [X, Y], Dir) -> true
    ; check(Plyr, Opp, State, [X, Y], Dirs)).
check(_, _, _, _, []) :- fail.

check_direction(Plyr, Opp, State, [X, Y], [DX, DY]) :-
    NX is X + DX,
    NY is Y + DY,
    inside_board(NX, NY),
    get(State, [NX, NY], Val),
    Val == Opp,
    cont_check(Plyr, Opp, State, [NX, NY], [DX, DY]).

cont_check(Plyr, Opp, State, [X, Y], [DX, DY]) :-
    NX is X + DX,
    NY is Y + DY,
    inside_board(NX, NY),
    get(State, [NX, NY], Val),
    (Val == Opp -> cont_check(Plyr, Opp, State, [NX, NY], [DX, DY])
    ; Val == Plyr).

inside_board(X, Y) :-
    X >= 0, X < 6,
    Y >= 0, Y < 6.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, Val) :-
    terminal(State), !,
    (winner(State, 1) -> Val = 100 ; winner(State, 2) -> Val = -100
    ; tie(State) -> Val = 0 ; true -> Val = 0).

h(State, Val) :-
    count(State, C1, C2),
    Val is C2 - C1.




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
