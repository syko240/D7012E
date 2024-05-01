:- ensure_loaded('othello.pl').
:- ensure_loaded('testboards.pl').

testBoardWinner1([
    [1,1,2,2,2,2],
    [1,1,2,2,2,2],
    [1,1,.,.,.,.],
    [2,2,.,.,.,.],
    [2,2,.,.,.,.],
    [2,2,.,.,.,.]
]).

testBoardWinner2([
    [2, 2, 2, 2, 2, 2],
    [2, 2, 2, 2, 2, 2],
    [1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1],
    [1, ., ., ., ., .],
    [., ., ., ., ., .]
]).

testBoardMoves([
    [., ., ., ., ., .],
    [., ., ., ., ., .],
    [., 2, 2, 2, ., .],
    [., 1, 1, 1, ., .],
    [., ., ., ., ., .],
    [., ., ., ., ., .]
]).

testBoard6([[1, 1, 2, ., ., .], 
            [2, 1, 1, 2, 1, .],
            [2, 2, 2, 2, 2, .], 
            [2, 1, 1, 1, ., .], 
            [., 1, 1, ., ., .], 
            [1, ., ., ., ., .] ]).

testBoardMoves2([
    [., ., ., ., ., .],
    [., 1, ., ., ., .],
    [., 1, 1, 2, ., .],
    [., 1, 1, 1, ., .],
    [., ., ., ., ., .],
    [., ., ., ., ., .]
]).

/*. . . . . .
. 1 . . . .
. 1 1 2 . .
. 1 1 1 . .
. . . . . .
. . . . . .*/

test_winner :-
    testBoardWinner1(Board1), winner(Board1, Winner1), writeln(Winner1),
    testBoardWinner2(Board2), winner(Board2, Winner2), writeln(Winner2),

    testBoard1(B1), winner(B1, W1), writeln(W1),
    testBoard2(B2), winner(B2, W2), writeln(W2),
    testBoard3(B3),winner(B3, W3), writeln(W3),
    onlyTwos(B4), winner(B4, W4), writeln(W4),
    onlyOnes(B5), winner(B5, W5), writeln(W5).

test_moves :-
    test_moves_initboard,
    test_moves_board1.

test_shackmove :-
    testBoard6(Board),
    moves(2, Board, MvList),
    ExpectedMoves = [[]],
    validate_moves('Board Player 1', MvList, ExpectedMoves).

test_moves_board2 :-
    testBoardMoves2(Board),
    moves(2, Board, MvList),
    ExpectedMoves = [],
    validate_moves('Board Player 2', MvList, ExpectedMoves).

test_moves_initboard :-
    initBoard(InitBoard),
    moves(1, InitBoard, MvList),
    ExpectedMoves = [[1, 3], [2, 4], [3, 1], [4, 2]],
    validate_moves('InitBoard Player 1', MvList, ExpectedMoves).

test_moves_board0 :-
    testBoardMoves(Board),
    moves(1, Board, MvList),
    ExpectedMoves = [[0, 1], [1, 1], [2, 1], [3, 1], [4, 1]],
    validate_moves('Board0 Player 1', MvList, ExpectedMoves).

test_moves_board1 :-
    testBoard1(Board),
    moves(1, Board, MvList1),
    ExpectedMoves1 = [[1,2], [2,1], [3,4], [4,3]],
    validate_moves('Board 1 Player 1', MvList1, ExpectedMoves1),
    moves(2, Board, MvList2),
    ExpectedMoves2 = [[0,0], [1,3], [2,4], [3,1], [4,2], [5,5]],
    validate_moves('Board 1 Player 2', MvList2, ExpectedMoves2).

validate_moves(TestName, MvList, ExpectedMoves) :-
    (MvList = ExpectedMoves -> format('~w: Passed~n', [TestName])
    ; format('~w: Failed. Expected ~w but got ~w~n', [TestName, ExpectedMoves, MvList])).

test_nextstate0 :-
    testBoardMoves(Board),
    showState(Board),
    nextState(1, [0, 1], Board, NewBoard, NextPlyr),
    format('~n'),
    showState(NewBoard).

test_nextstate :-
    initBoard(Board),
    showState(Board),
    test_nextstate1(Board, NewBoard),
    format('~n'),
    showState(NewBoard),
    test_nextstate2(NewBoard, NewBoard1),
    format('~n'),
    showState(NewBoard1).

test_nextstate1(Board, NewBoard) :-
    nextState(1, [1, 3], Board, NewBoard, NextPlyr).

test_nextstate2(Board, NewBoard) :-
    nextState(2, [1, 2], Board, NewBoard, NextPlyr).
