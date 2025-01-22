
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

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
    nl,
    nl,
    nl,
    write('Welcome to Connect4IF.'),
    read_players,
    output_players
    .

initialize :-
    blank_mark(E),
    asserta( Board (
    [   [E,E,E,E,E,E],
        [E,E,E,E,E,E],
        [E,E,E,E,E,E],
        [E,E,E,E,E,E],
        [E,E,E,E,E,E],
        [E,E,E,E,E,E],
        [E,E,E,E,E,E]]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'y'), 
    !,
    run
    .

% l'ordinateur lit les prédicats dans l'ordre. Quand il rencontre un ! il s'arrête et passe au prédicat suivant
read_play_again(V) :-
    nl,
    nl,
    write('Play again (y/n)? '),
    read(V),
    (V == 'y' ; V == 'n'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter y or n.'),
    read_play_again(V) %appelle le premier prédicat qui a ce nom
    .

read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .

human_playing(M) :- 
    (M == 'x'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter x or o.'),
    set_players(1)
    .


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
%

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
    move(B, S, M, B2),                    % Apply the move to the board.
    !.                                    % Cut to prevent backtracking.

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
    random_int_1n(7,S), %version 1: l'odinateur joue au hasard
    % minimax(0, B, M, S, U),
    move(B, S, M, B2),

    nl, nl,
    write('Computer places '), write(M),
    write(' in square '), write(S), write('.')    .

%.......................................
% moves
%.......................................
% Retrieves a list of available moves (empty squares) on the board.
moves(B, L) :-
    \+ win(B, x),                         % If either player has already won, there are no valid moves.
    \+ win(B, o),
    blank_mark(E),
    findall(N, square(B, N, E), L),       % Find all blank squares.
    L \= [].                              % Ensure the list of moves is not empty.


%.......................................
% rectangle
%.......................................
% Retrieves all the top values of the columns in a given board that match M.
rectangle(B, Col, M) :- 
    nth1(Col, B, Column),                 % Get the column at position Col.
    nth1(6, Column, M).                   % Check the topmost value of the column.

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
    S = 4,
    !.

minimax(D,B,M,S,U) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,S,U),  %%% recursively determine the best available move
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position
minimax(D,B,M,S,U) :-
    utility(B,U)      
    .

%.......................................
% place in column
%.......................................
% appends the Mark in a column
place_in_column([_|Rest], M, [M|Rest]) :- !.  % Replace the first empty slot with M.
place_in_column([H|T], M, [H|NewT]) :-
    place_in_column(T, M, NewT).  % Recurse until an empty slot is found.

%.......................................
% replace column
%.......................................
% replaces an entier column in a board
replace_column([_|T], 1, NewCol, [NewCol|T]) :- !.  % Replace the first column (Nth = 1).
replace_column([H|T], N, NewCol, [H|NewT]) :-
    N > 1,
    N1 is N - 1,
    replace_column(T, N1, NewCol, NewT).  % Recurse to find the correct column.

%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in column S on board B and return the resulting board B2)
move(Board, ColNum, M, NewBoard) :-
    nth1(ColNum, Board, Col),  % Get the `ColNum`th column from the board.
    place_in_column(Col, M, NewCol),  % Place the mark `M` in the column.
    replace_column(Board, ColNum, NewCol, NewBoard).  % Update the board with the new column.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
    !
    .

output_winner(B) :-
    win(B,x),
    write('X wins.'),
    !
    .

output_winner(B) :-
    win(B,o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .

%.......................................
% Affiche le plateau de jeu
%.......................................
output_board(Board) :-
    nl,
    output_rows(Board, 6), % On commence par la 6ème ligne (le bas) sinon ça s'affiche à l'envert
    output_column_numbers, % Affiche les numéros de colonnes
    nl.

%.......................................
% Affiche les lignes du bas vers le haut
%.......................................
output_rows(_, 0) :- !.
output_rows(Board, Row) :-
    output_row(Board, Row),
    NextRow is Row - 1,
    output_rows(Board, NextRow).

%.......................................
% Affiche une ligne donnée
%.......................................
output_row(Board, Row) :-
    write('|'),
    output_cells(Board, Row, 1).

%.......................................
% Parcourt les colonnes et affiche la cellule correspondante
%.......................................
output_cells(_, _, 8) :-
    nl, !.
output_cells(Board, Row, Col) :-
    nth1(Col, Board, Column), % Récupère la colonne actuelle
    nth1(Row, Column, Cell), % Récupère la cellule (contenu) à la ligne Row
    output_square(Cell), % Affiche le contenu de la cellule
    write('|'),
    NextCol is Col + 1,
    output_cells(Board, Row, NextCol).

%.......................................
% Affiche une cellule (X, O, ou vide)
%.......................................
output_square(Cell) :-
    (var(Cell) -> write(' '); write(Cell)), !. % Affiche un espace si la cellule est vide, sinon affiche le contenu (X, O, etc.)

%.......................................
% Affiche les numéros des colonnes (1 à 7)
%.......................................
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
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .
%how to call the function
%random_int_1n(7,R).
