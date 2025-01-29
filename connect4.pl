
/*

The following conventions are used in this program...

Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 9 item list representing a 3x3 matrix)
    each "square" on the board can contain one of 3 values: x ,o, or e (for empty)
S - the number of a square on the board (1 - 9)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...) 
     and myrule3(...) is meant to be called from myrule2(...)


There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board 
asserta( player(P, Type) ) - indicates which players are human/computer.

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% Initialization & manage I/O
%.......................................

run :-
    hello,          %%% Display welcome message, initialize game
    play(1),        %%% Play the game starting with player 1
    goodbye.        %%% Display end of game message

run :-
    goodbye.

hello :-
    initialize,
    nl, nl,
    write('Welcome to Connect4IF.'),
    read_players,
    output_players.

initialize :-
    blank_mark(E),
    asserta(
        board([
            [E, E, E, E, E, E],
            [E, E, E, E, E, E],
            [E, E, E, E, E, E],
            [E, E, E, E, E, E],
            [E, E, E, E, E, E],
            [E, E, E, E, E, E],
            [E, E, E, E, E, E]
        ])
    ).  %%% create a blank board

Tgoodbye :-
    board(B),
    nl, nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'y'), !,
    run.

% l'ordinateur lit les prédicats dans l'ordre. Quand il rencontre un ! il s'arrête et passe au prédicat suivant
read_play_again(V) :-
    nl, nl,
    write('Play again (y/n)? '),
    read(V),
    (V == 'y' ; V == 'n'), !.

read_play_again(V) :-
    nl, nl,
    write('Please enter y or n.'),
    read_play_again(V). %appelle le premier prédicat qui a ce nom

read_players :-
    nl, nl,
    write('Number of human players? '),
    read(N),
    set_players(N).

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !.

set_players(1) :-
    nl,
    write('Is human playing x or o (x moves first)? '),
    read(M),
    human_playing(M), !.

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !.

set_players(_) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players.

human_playing(M) :- 
    (M == 'x'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !.

human_playing(M) :- 
    (M == 'o'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !.

human_playing(_) :-
    nl,
    write('Please enter x or o.'),
    set_players(1).


%.......................................
% play
%.......................................
% main function

play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .

%.......................................
% make_move
%.......................................
% Requests the next move from human/computer, 
% then applies that move to the given board.

make_move(P, B) :-
    player(P, Type),
    make_move2(Type, P, B, B2),  % Delegate the move based on player type (human or computer).
    retract(board(_)),           % Update the current board.
    asserta(board(B2)).

% Human player makes a move.
make_move2(human, P, B, B2) :-
    nl, nl,
    write('Player '), write(P), write(' move? '),
    read(S),                              % Read the square index from the player.
    moves(B, AvailableMoves),             % Get the list of available moves.
    member(S, AvailableMoves),            % Check if the selected square is valid.
    player_mark(P, M),
    move(B, S, M, B2), !.                   % Apply the move to the board and cuts to prevent backtracking

% Handle invalid move by human player.
make_move2(human, P, B, B2) :-
    nl, nl,
    write('Invalid move. Please select a valid, empty column.'),
    make_move2(human, P, B, B2).          % Retry until the human makes a valid move.

% Computer player makes a move using minimax algorithm.
make_move2(computer, P, B, B2) :-
    nl, nl,
    write('Computer is thinking about its next move...'),
    player_mark(P, M),
    random_ia(B,S),    %version 1: l'odinateur joue au hasard
    % minimax(0, B, M, S, U),
    move(B, S, M, B2),
    nl, nl,
    write('Computer places '), write(M),
    write(' in square '), write(S), write('.').

%.......................................
% moves
%.......................................
% Retrieves a list of available moves (empty squares) on the board.
moves(B, L) :-
    \+ win(B, x),                         % If either player has already won, there are no valid moves.
    \+ win(B, o),
    blank_mark(E),
    findall(N, rectangle(B,N,E), L),      % Find all empty squares in the top row of each column.
    L \= [].                              % Ensure the list of moves is not empty.


%.......................................
% rectangle
%.......................................
% Retrieves all the top values of the columns in a given board that match M.
rectangle(B, Col, M) :- 
    nth1(Col, B, Column),                 % Get the column at position Col.
    nth1(6, Column, M).                   % Check the topmost value of the column.

%.......................................
% win
%.......................................

isconsecutive(List, Player, Length) :-
    length(SubList, Length),
    append(_, SubListRest, List),
    append(SubList, _, SubListRest),
    maplist(=(Player), SubList).

transpose([], []).
transpose([[] | _], []).
transpose(Matrix, [Row | TransposedTail]) :-
    extract_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, TransposedTail).

extract_column([], [], []).
extract_column([[H | T] | Rows], [H | Column], [T | RestRows]) :-
    extract_column(Rows, Column, RestRows).

horizontal_win(B, M) :- 
    member(Row, B),
    isconsecutive(Row, M, 4).

vertical_win(B, M) :- 
    transpose(B, TBoard),
    member(Row, TBoard),
    isconsecutive(Row, M, 4).

diagonal_win_check(B, M) :-
    length(B, Rows),
    length(B, Cols),
    RowStartMax is Rows - 3,
    ColStartMax is Cols - 3,
    between(0, RowStartMax, RowStart),
    between(0, ColStartMax, ColStart),
    diagonal_win_from(B, RowStart, ColStart, M).

diagonal_win_check(B, M) :-
    length(B, Rows),
    length(B, Cols),
    RowStartMax is Rows - 3,
    ColStartMax is Cols - 3,
    between(0, RowStartMax, RowStart),
    ColEndMax is Cols - 1,
    between(0, ColEndMax, ColEnd),
    diagonal_win_from_reverse(B, RowStart, ColEnd, M).

diagonal_win_from(B, RowStart, ColStart, M) :-
    nth0(RowStart, B, Row1),
    nth0(ColStart, Row1, M),
    Row2Start is RowStart + 1, Col2Start is ColStart + 1,
    nth0(Row2Start, B, Row2),
    nth0(Col2Start, Row2, M),
    Row3Start is Row2Start + 1, Col3Start is Col2Start + 1,
    nth0(Row3Start, B, Row3),
    nth0(Col3Start, Row3, M),
    Row4Start is Row3Start + 1, Col4Start is Col3Start + 1,
    nth0(Row4Start, B, Row4),
    nth0(Col4Start, Row4, M).

diagonal_win_from_reverse(B, RowStart, ColEnd, M) :-
    nth0(RowStart, B, Row1),
    nth0(ColEnd, Row1, M),
    Row2Start is RowStart + 1, Col2Start is ColEnd - 1,
    nth0(Row2Start, B, Row2),
    nth0(Col2Start, Row2, M),
    Row3Start is Row2Start + 1, Col3Start is Col2Start - 1,
    nth0(Row3Start, B, Row3),
    nth0(Col3Start, Row3, M),
    Row4Start is Row3Start + 1, Col4Start is Col3Start - 1,
    nth0(Row4Start, B, Row4),
    nth0(Col4Start, Row4, M).

win(B, M) :- 
    horizontal_win(B, M);
    vertical_win(B, M);
    diagonal_win_check(B, M).

%.......................................
% game_over
%.......................................
% determines when the game is over
game_over(P, B) :-
    game_over2(P, B).

% game is over if opponent wins
game_over2(P, B) :-
    opponent_mark(P, M),
    win(B, M).

% game is over if there are no blank squares left
game_over2(P, B) :-
    moves(B, L),
    L == [].


%.......................................
% IA random
%.......................................
% The randomIA algorithm plays.
random_ia(B, S):-
    random_int_1n(7,S),                   % Read the square index from the player.
    moves(B, AvailableMoves),             % Get the list of available moves.
    member(S, AvailableMoves).           % Check if the selected square is valid.

% Handle invalid move by computer.
random_ia(B, S):-
    nl, nl,
    write('Invalid random number. New try.'),
    random_ia(B, S).         % Retry until the coputer makes a valid move.
%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.

% The best opening move is always dead center.
minimax(D,[ [E,E,E,E,E,E], 
            [E,E,E,E,E,E], 
            [E,E,E,E,E,E], 
            [E,E,E,E,E,E], 
            [E,E,E,E,E,E], 
            [E,E,E,E,E,E], 
            [E,E,E,E,E,E] ],
            M,S,U) :-   
    blank_mark(E),
    S = 4, !.

minimax(D,B,M,S,U) :-
    D2 is D + 1,
    moves(B,L), !,          %%% get the list of available moves
    best(D2,B,M,L,S,U), !.  %%% recursively determine the best available move

% if there are no more available moves, 
% then the minimax value is the utility of the given board position
minimax(D,B,M,S,U) :-
    utility(B,U).

%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in column S on board B and return the resulting board B2)
move(B, ColNum, M, Newboard) :-
    nth1(ColNum, B, Col),  % Get the `ColNum`th column from the board.
    place_in_column(Col, M, NewCol),  % Place the mark `M` in the column.
    replace_column(B, ColNum, NewCol, Newboard).  % Update the board with the new column.

% Place in column - places the mark `M` in the first empty slot in the column.
place_in_column([E|Rest], M, [M|Rest]) :-  % If the head is empty, place the mark `M`.
    E == 'e', !. % Only replace empty slot ('e').
place_in_column([H|T], M, [H|NewT]) :-  % Otherwise, recursively check the tail.
    place_in_column(T, M, NewT).

% replaces an entier column in a board
replace_column([_|T], 1, NewCol, [NewCol|T]) :- !.  % Replace the first column (Nth = 1).
replace_column([H|T], N, NewCol, [H|NewT]) :-
    N > 1,
    N1 is N - 1,
    replace_column(T, N1, NewCol, NewT).  % Recurse to find the correct column.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '), write(V1),  %%% either human or computer
    nl,
    player(2, V2),
    write('Player 2 is '), write(V2), !.  %%% either human or computer

output_winner(B) :-
    win(B,x),
    write('X wins.'), !.

output_winner(B) :-
    win(B,o),
    write('O wins.'), !.

output_winner(_) :-
    write('No winner.').

% Affiche le plateau de jeu
output_board(B) :-
    nl,
    output_rows(B, 6), % On commence par la 6ème ligne (le bas) sinon ça s'affiche à l'envert
    output_column_numbers, % Affiche les numéros de colonnes
    nl.

% Affiche les lignes du bas vers le haut
output_rows(_, 0) :- !.
output_rows(B, Row) :-
    output_row(B, Row),
    NextRow is Row - 1,
    output_rows(B, NextRow).

% Affiche une ligne donnée
output_row(B, Row) :-
    write('|'),
    output_cells(B, Row, 1).

% Parcourt les colonnes et affiche la cellule correspondante
output_cells(_, _, 8) :-
    nl, !.
output_cells(B, Row, Col) :-
    nth1(Col, B, Column), % Récupère la colonne actuelle
    nth1(Row, Column, Cell), % Récupère la cellule (contenu) à la ligne Row
    output_square(Cell), % Affiche le contenu de la cellule
    write('|'),
    NextCol is Col + 1,
    output_cells(B, Row, NextCol).


% Affiche une cellule (X, O, ou vide)
% Si la cellule est vide, affiche un espace
% Sinon, si la cellule est un X, affiche en rouge
% Sinon, si la cellule est un O, affiche en bleu
output_square(X) :-
    X == x, !,
    ansi_format([fg(red)], 'x', []). % Affiche un X en rouge
output_square(O) :-
    O == o, !,
    ansi_format([fg(blue)], 'o', []). % Affiche un O en bleu
output_square(E) :-
    E == e, !,
    write(' '). % Affiche un espace

% Affiche les numéros des colonnes (1 à 7)
output_column_numbers :-
    write('---------------'), nl,
    write(' 1 2 3 4 5 6 7'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% generate random number for the AI to play
% random_int_1n
%.......................................
% returns a random integer from 1 to N
% eg : random_int_1n(7,R).
random_int_1n(N, V) :-
    V is random(N) + 1, !.
