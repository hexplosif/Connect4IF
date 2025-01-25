
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
%%%     IMPORTS MODULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(pce)). % Import the PCE library for GUI elements

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
    hello.          %%% Display welcome message, initialize game
    %play(1),        %%% Play the game starting with player 1
    %goodbye.        %%% Display end of game message

run :-
    goodbye.

hello :-
    initialize,
    nl, nl,
    write('Welcome to Connect4IF.'),
    read_players.

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

goodbye :-
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
    n_players_window(W).

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ),
    play(1), !.

set_players(1) :-
    nl,
    write('Is human playing x or o (x moves first)? '),
    choose_mark_window(W), !.

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ),
    play(1), !.

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
    player(P, Type),
    Type == human,
    write('\n--Player type: '), write(Type), nl,
    board(B),
    output_board(B),
    board_window(W, P, B).

play(P) :-
    player(P, Type),
    write('\n--Player type: '), write(Type), nl,
    Type == computer, !,
    write('\n--Player type: '), write(Type), nl,
    board(B),
    output_board(B),
    make_move(P, B),
    next_player(P, P2),
    play(P2).

%.......................................
% make_move
%.......................................
% Requests the next move from human/computer, 
% then applies that move to the given board.

% Computer player makes a move.
make_move(P, B) :-   
    player(P, Type),
    write('\n--Player type: '), write(Type), nl,
    make_move2(Type, P, B, B2),  % Delegate the move based on player type (human or computer).
    write('\n--Player '), write(P), write(' places '), write(M),
    retract(board(_)),           % Update the current board.
    asserta(board(B2)).

% Human player makes a move.
make_move(P, B, S) :-   
    player(P, Type),
    make_move2(Type, P, S, B, B2),  % Delegate the move based on player type (human or computer).
    retract(board(_)),           % Update the current board.
    asserta(board(B2)).

% Human player makes a move.
make_move2(human, P, S, B, B2) :-
    nl, nl,
    moves(B, AvailableMoves), 
    member(S, AvailableMoves),            % Check if the selected square is valid.
    player_mark(P, M),
    move(B, S, M, B2), !.                   % Apply the move to the board and cuts to prevent backtracking


% Handle invalid move by human player.
make_move2(human, P, S, B, B2) :-
    nl, nl,
    write('Invalid move. Please select a valid, empty column.'),
    make_move2(human, P, S, B, B2).          % Retry until the human makes a valid move.


% Computer player makes a move using minimax algorithm.
make_move2(computer, P, B, B2) :-
    nl, nl,
    write('Computer is thinking about its next move...'),
    player_mark(P, M),
    random_int_1n(7, S), %version 1: l'odinateur joue au hasard
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
    findall(N, rectangle(B,N,E), L), 
    L \= [].
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
    transpose(B, TB),
    member(Row, TB),
    isconsecutive(Row, M, 4).

win(B, M) :- 
    horizontal_win(B, M);
    vertical_win(B, M).

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
    L == [],
    write('No more moves left.'), nl.

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
output_board(Board) :-
    nl,
    output_rows(Board, 6), % On commence par la 6ème ligne (le bas) sinon ça s'affiche à l'envert
    output_column_numbers, % Affiche les numéros de colonnes
    nl.

% Affiche les lignes du bas vers le haut
output_rows(_, 0) :- !.
output_rows(Board, Row) :-
    output_row(Board, Row),
    NextRow is Row - 1,
    output_rows(Board, NextRow).

% Affiche une ligne donnée
output_row(Board, Row) :-
    write('|'),
    output_cells(Board, Row, 1).

% Parcourt les colonnes et affiche la cellule correspondante
output_cells(_, _, 8) :-
    nl, !.
output_cells(Board, Row, Col) :-
    nth1(Col, Board, Column), % Récupère la colonne actuelle
    nth1(Row, Column, Cell), % Récupère la cellule (contenu) à la ligne Row
    output_square(Cell), % Affiche le contenu de la cellule
    write('|'),
    NextCol is Col + 1,
    output_cells(Board, Row, NextCol).


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
%%%     GUI INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

menu_window(W) :-
    new(W, dialog('Connect4IF')),
    send(W, append, new(button('Start', message(@prolog, run_game, W)))),
    send(W, append, new(button('Quit', message(W, destroy)))),
    send(W, position, point(200, 100)),
    send(W, open).

run_game(W) :-
    send(W, destroy),
    run.

n_players_window(W) :-
    new(W, dialog('Connect4IF')),
    send(W, append, new(button('0', message(@prolog, set_num_players, 0, W)))),
    send(W, append, new(button('1', message(@prolog, set_num_players, 1, W)))),
    send(W, append, new(button('2', message(@prolog, set_num_players, 2, W)))),
    send(W, position, point(200, 100)),
    send(W, open).

set_num_players(N, W) :-
    set_players(N),
    send(W, destroy).

choose_mark_window(W) :-
    new(W, dialog('Connect4IF')),
    send(W, append, new(button('X', message(@prolog, set_mark, x, W)))),
    send(W, append, new(button('O', message(@prolog, set_mark, o, W)))),
    send(W, position, point(200, 100)),
    send(W, open).

set_mark(M, W) :-
    human_playing(M),
    send(W, destroy),
    play(1).

board_window(W, P, B) :-
    new(W, dialog('Connect4IF')),
    write('\n--Player '), write(P), write(' turn.'), nl,
    send(W, size, size(370, 380)),
    send(W, position, point(200, 100)),
    send(W, open),
    send(W, background, colour(black)),
    draw_board(W, P, B, 6, 7).

% Dessine le plateau sur la fenêtre
draw_board(W, P, B, Rows, Cols) :-
    draw_cells(W, B, Rows, Cols, Rows, 1),
    draw_buttons(W, P, B, Cols).

% Parcourt les cellules pour les dessiner
draw_cells(_, _, 0, _, _, _) :- !.
draw_cells(W, B, Row, Cols, TotalRows, TotalCols) :-
    draw_row(W, B, Row, Cols, TotalRows, TotalCols),
    NextRow is Row - 1,
    draw_cells(W, B, NextRow, Cols, TotalRows, TotalCols).

% Dessine une ligne donnée
draw_row(_, _, _, 0, _, _) :- !.
draw_row(W, B, Row, Col, TotalRows, TotalCols) :-
    nth1(Col, B, Column),
    nth1(Row, Column, M),
    CellY is (TotalRows - Row) * 50 + 15,
    CellX is (Col - 1) * 50 + 15,
    draw_cell(W, M, CellX, CellY),
    NextCol is Col - 1,
    draw_row(W, B, Row, NextCol, TotalRows, TotalCols).

% Dessine une cellule donnée
draw_cell(W, M, X, Y) :-
    (   M == x
    ->  new(Circle, circle(40)),
        send(Circle, fill_pattern, colour(red)),
        send(W, display, Circle, point(X, Y))
    ;   M == o
    ->  new(Circle, circle(40)),
        send(Circle, fill_pattern, colour(blue)),
        send(W, display, Circle, point(X, Y))
    ;   new(Box, box(40, 40)),
        send(Box, fill_pattern, colour(grey20)),
        send(W, display, Box, point(X, Y))
    ).

% Dessine les boutons pour chaque colonne, sous le plateau
draw_buttons(W, P, B, Cols) :-
    ButtonY is 50 * 6 + 15, % Position sous les cellules
    draw_buttons(W, P, B, 1, Cols, ButtonY).

draw_buttons(_, P, B, Col, TotalCols, _) :-
    Col > TotalCols, !.
draw_buttons(W, P, B, Col, TotalCols, ButtonY) :-
    X is (Col - 1) * 50 + 15,
    new(Button, button(Col, message(@prolog, play_move, W, P, Col))),
    send(Button, colour, colour(white)),
    send(W, display, Button, point(X, ButtonY)),
    NextCol is Col + 1,
    draw_buttons(W, P, B, NextCol, TotalCols, ButtonY).

play_move(W, P, Col) :-
    board(B),
    not(game_over(P, B)), !,
    make_move(P, B, Col), !,
    send(W, destroy), !,
    next_player(P, P2), !,
    play(P2), !.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- menu_window(W).