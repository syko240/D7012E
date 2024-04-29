testBoardWinner1([
    ['1', '1', '2', '2', '2', '2'],
    ['1', '1', '2', '2', '2', '2'],
    ['1', '1', '.', '.', '.', '.'],
    ['2', '2', '.', '.', '.', '.'],
    ['2', '2', '.', '.', '.', '.'],
    ['2', '2', '.', '.', '.', '.']
]).

testBoardWinner2([
    ['2', '2', '2', '2', '2', '2'],
    ['2', '2', '2', '2', '2', '2'],
    ['1', '1', '1', '1', '1', '1'],
    ['1', '1', '1', '1', '1', '1'],
    ['1', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.']
]).

test_winner :-
    testBoardWinner1(Board1),
    (   winner(Board1, Winner1)
    ->  write('Winner of Board 1: '), writeln(Winner1)
    ;   writeln('No winner for Board 1')),
    writeln(Winner1),

    testBoardWinner2(Board2),
    (   winner(Board2, Winner2)
    ->  write('Winner of Board 2: '), writeln(Winner2)
    ;   writeln('No winner for Board 2')),
    writeln(Winner2).

    %format('Winner of board 1: ~w~nWinner of board 2: ~w', [Winner1, Winner2]).