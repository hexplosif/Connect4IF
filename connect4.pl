
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

score_board([
    [3, 4, 5, 5, 4, 3],
    [4, 6, 8, 8, 6, 4],
    [5, 8, 10, 10, 8, 5],
    [7, 10, 13, 13, 10, 7],
    [5, 8, 10, 10, 6, 4],
    [4, 6, 8, 8, 6, 4],
    [3, 4, 5, 5, 4, 3]
]).

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

goodbye :-
    board(B),
    nl, nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_,_)),
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
    write('What AI do you want for the first AI ? (1, 2, 3, 4)\n'),
    write('1: Random AI\n'),
    write('2: Weighted-grid AI\n'),
    write('3: Offensive AI\n'),
    write('4: Defensive AI\n'),
    read(AI1),
    write('What AI do you want for the second AI ? (1, 2, 3, 4)\n'),
    write('1: Random AI\n'),
    write('2: Weighted-grid AI\n'),
    write('3: Offensive AI\n'),
    write('4: Defensive AI\n'),
    read(AI2),
    asserta( player(1, computer, AI1) ),
    asserta( player(2, computer, AI2) ), !.

set_players(1) :-
    nl,
    write('Is human playing x or o (x moves first)? \n'),
    read(M),
    write('What AI do you want to play against? (1, 2, 3, 4)\n'),
    write('1: Random AI\n'),
    write('2: Weighted-grid AI\n'),
    write('3: Offensive AI\n'),
    write('4: Defensive AI\n'),
    read(AI),
    write('AI: '), write(AI),
    human_playing(M, AI), !.

set_players(2) :- 
    asserta( player(1, human, 0) ),
    asserta( player(2, human, 0) ), !.

set_players(_) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players.

human_playing(M, AI) :- 
    (M == 'x'),
    asserta( player(1, human, 0) ),
    asserta( player(2, computer, AI) ), !.

human_playing(M, AI) :- 
    (M == 'o'),
    asserta( player(1, computer, AI) ),
    asserta( player(2, human, 0) ), !.

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

% Vérifie si une ligne ne contient pas 'e'
row_full([]).
row_full([H|T]) :- H \= 'e', row_full(T).

% Vérifie si tout le plateau est rempli (aucune ligne ne contient 'e')
board_full([]).
board_full([Row|Rest]) :- row_full(Row), board_full(Rest).

make_move(P, B) :-
    player(P, Type, AI),
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
    player(P,Z,IA),
    % write(IA),
    jeu_IA(IA, B, M, S, U),
    move(B, S, M, B2),
    nl, nl,
    write('Computer places '), write(M),
    write(' in column '), write(S), write('.').

jeu_IA(1, B, M, S, U):-
    random_ia(B,S).    %version 1: l'odinateur joue au hasard

jeu_IA(2, B, M, S, U):-
    computer_best_score_move(B,S).

jeu_IA(_, B, M, S, U):-
    minimax(0, B, M, S, U). %version 3 ou 4: l'ordinateur joue avec minimax


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
% AI random
%.......................................
% The randomIA algorithm plays.
random_ia(B, S):-
    random_int_1n(7,S),                   % Read the square index from the player.
    moves(B, AvailableMoves),             % Get the list of available moves.
    member(S, AvailableMoves),
    !.           % Check if the selected square is valid.

% Handle invalid move by computer.
random_ia(B, S):-
    nl, nl,
    write('Invalid random number. New try.'),
    random_ia(B, S).         % Retry until the coputer makes a valid move.

%.......................................
% AI 2
% Computer plays best score
% On donne des scores aux cases pour savoir si elles offrent une grande possibilité de coups gagnants
% Peu efficace car très déterministe -> un peu mieux que l'aléatoire  '
%.......................................
% 
computer_best_score_move(B,S):-
    moves(B, AvailableMoves),             % Get the list of available moves.
    get_list_scores(AvailableMoves, L),
    maximumListe(L,Max), !,               % find the maximum score in the list 
    nth1(Imax,L,Max), !,                  % find the index of the maximum score in the list
    nth1(Imax,AvailableMoves,S)           % find the best move
.

get_list_scores([],[]).
get_list_scores([Available_T|Available_Q], [Score|L]):-
    get_list_scores(Available_Q, L),
    calculer_score_location(Available_T, Score)     % Pour chaque elemennt de AvailableMoves on renvoie le score associé à l'emplacement et on le stocke dans la liste L  '
    .

calculer_score_location(Available_T, Score):-
    score_board(SB),
    board(B),
    nth1(Available_T, B, ColB),             % Get the column number Available_T of the board
    nth1(Available_T, SB, ColSB),           % Get the column number Available_T of the score_board
    nth1(I,ColB,e), !,                      % Get the first empty mark of the column ColB of the mark
    nth1(I,ColSB,Score)                     % Get the score of the empty mark found above
    .



%.......................................
% evaluation with two combination
%.......................................
% tools necessary to determine the evaluation of a given board position
%
horizontal_evaluation(Board, Comb, U, NewU) :- 
    findall(_, (member(Row, Board), append([_, Comb, _], Row)), Matches),
    length(Matches, Count),
    NewU is U + Count
    .

vertical_evaluation(Board, Comb, U, NewU) :- 
    transpose(Board, TBoard),
    findall(_, (member(Row, TBoard), append([_, Comb, _], Row)), Matches),
    length(Matches, Count),
    NewU is U + Count
    .

diagonal_evaluation(Board, Comb, U, NewU) :- 
    diagonals(Board, Diags),
    findall(_, (member(Diag, Diags), append([_, Comb, _], Diag)), Matches),
    length(Matches, Count),
    NewU is U + Count
    .

% Récupération des diagonales d'une grille
diagonals(Board, Diags) :-
    findall(Diag, diagonal(Board, Diag), Diags).

diagonal(Board, Diag) :-
    length(Board, N),
    between(0, N, K),
    findall(Elem, (
        between(0, N, I),
        J is I + K,
        nth0(I, Board, Row),
        nth0(J, Row, Elem)
    ), Diag),
    Diag \= [].

diagonal(Board, Diag) :-
    length(Board, N),
    between(0, N, K),
    findall(Elem, (
        between(0, N, I),
        J is I - K,
        nth0(I, Board, Row),
        nth0(J, Row, Elem)
    ), Diag),
    Diag \= [].

% .......................................
% evaluation compter les doubles et triples
% .......................................
% determines the value of a given board position
%
combinationX([ ['x','x','x'], 
                ['x','x'] ]).

combinationO([ ['o','o','o'], 
                ['o','o'] ]).

evaluate([],B).

evaluate([H|T],B) :-
    % write('debut evaluation'),
    valeurU(U1),
    horizontal_evaluation(B,H,U1,NewU1),
    vertical_evaluation(B,H,NewU1,NewU2),
    diagonal_evaluation(B,H,NewU1,NewU3),
    retract(valeurU(_)),
    asserta(valeurU(NewU3)),
    evaluate(T,B)
    .

%.......................................
% defensive_evaluation
%.......................................


% Évalue un plateau en se basant sur une approche défensive.
% Valorise les positions qui bloquent les opportunités de l'adversaire.

% Détecte une combinaison dangereuse pour l'adversaire

combinationDanger(['e', 'e', M, M], M).
combinationDanger([M, M, 'e', 'e'], M).
combinationDanger(['e', M, M, 'e'], M).
combinationDanger([ M, 'e', M, 'e'], M).
combinationDanger(['e', M, 'e', M], M).
combinationDanger([M, 'e', 'e', M], M).

combinationDanger([M, M, M, 'e'], M).
combinationDanger(['e', M, M, M], M).
combinationDanger([M, 'e', M, M], M).
combinationDanger([M, M, 'e', M], M).

% Évaluation horizontale défensive
defensive_horizontal_evaluation(Board, Opponent, U, NewU) :-
    findall(_, (
        member(Row, Board),
        append([_, Comb, _], Row), 
        combinationDanger(Comb, Opponent)
    ), Matches),
    length(Matches, Count),
    NewU is U + Count * 10.

% Évaluation verticale défensive
defensive_vertical_evaluation(Board, Opponent, U, NewU) :-
    transpose(Board, TBoard),
    defensive_horizontal_evaluation(TBoard, Opponent, U, NewU).

% Évaluation diagonale défensive
defensive_diagonal_evaluation(Board, Opponent, U, NewU) :-
    findall(_, (
        diagonals(Board, Diags),
        member(Diag, Diags),
        append([_, Comb, _], Diag), 
        combinationDanger(Comb, Opponent)
    ), Matches),
    length(Matches, Count),
    NewU is U + Count * 10.

% Évaluation globale défensive
defensive_evaluation(Board, U, M) :-
    inverse_mark(M, Opponent),
    U2 = 0,
    defensive_horizontal_evaluation(Board, Opponent, U2, NewU1),
    defensive_vertical_evaluation(Board, Opponent, NewU1, NewU),
    defensive_diagonal_evaluation(Board, Opponent, NewU, U).

%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U,M,AI) :-
    win(B,'x'),
    U = 1000000, 
    !
    .

utility(B,U,M,AI) :-
    win(B,'o'),
    U = (-1000000), 
    !
    .

utility(B,U,M,AI) :-
    board_full(B),
    U = 0, 
    !
    .

utility(B,U,'x',3) :-
    asserta(valeurU(0)),
    % write('utility 1'),nl,
    retract(valeurU(_)),
    asserta(valeurU(0)),
    combinationX(C),
    evaluate(C,B),
    valeurU(U1),
    U = U1,
    !
    .

utility(B,U,'o',3) :-
    asserta(valeurU(0)),
    % write('utility 2 first'),nl,
    retract(valeurU(_)),
    % write('utility 2'),nl,
    asserta(valeurU(0)),
    % write('utility 2'),nl,
    combinationO(C),
    % write(C),
    evaluate(C,B),
    valeurU(U1),
    U = -U1,
    !
    .

utility(B, U, M, 4) :-
    defensive_evaluation(B, U, M).

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
    % write('minimax 2'),nl,
    D2 is D + 1,
    moves(B,L), !,          %%% get the list of available moves
    best(D2,B,M,L,S,U), !.  %%% recursively determine the best available move

% if there are no more available moves, 
% then the minimax value is the utility of the given board position
minimax(D,B,M,S,U) :-
    player(A, computer, AI),
    utility(B,U,M,AI).

%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if max depth is reached
% if there is only one move left in the list...

best(4,B,M,[S1],S,U) :-
    % write('best 1'),nl,
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    player(A, computer, AI),
    utility(B2,U,M2,AI),  %%% then search for the utility value of that move.
    S = S1, !,
    % output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(4,B,M,[S1|T],S,U) :-
    % write('best 2'),nl,
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    player(A, computer, AI),
    utility(B2,U1,M2,AI),      %%% recursively search for the utility value of that move,
    best(4,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    % output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    % write('best 3'),nl,
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    % output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    % write('best 4'),nl,
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    % write(B),nl,nl,
    inverse_mark(M,M2),
    !,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    % output_value(D,S1,U1),     
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .

%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,     S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1, 
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),    
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    R < 6,
    S = S1,
    U = U1, 
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
    .

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
    write('Player 1: '),
    player(1, Type1, AI1),
    write(Type1), write(' '), write(AI1), nl,
    write('Player 2: '),
    player(2, Type2, AI2),
    write(Type2), write(' '), write(AI2), nl.

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


%.......................................
% find the maximum value in a list
%.......................................

maximumListe([X],X). %vrai si X est le seul element de la liste
maximumListe([T|Q],X):-maximumListe(Q,M), (M<T -> X=T ; X=M). %X est assigné au plus grand des deux (à chaque fois)



% Run multiple games between two Random AIs and track results
run_simulation(N, XWins, OWins, Draws) :-
    run_games(N, 0, 0, 0, XWins, OWins, Draws).

% Base case: No more games to play, return results
run_games(0, X, O, D, XFinal, OFinal, DFinal) :- !,
    write('Results:'), nl,
    write('Player X wins: '), write(X), nl,
    write('Player O wins: '), write(O), nl,
    write('Draws: '), write(D), nl.

% Play a game, update results, and recurse
run_games(N, X, O, D, XFinal, OFinal, DFinal) :-
    initialize,                     % Reset board for a new game
    asserta( player(1, computer, 1) ), % Assign Random AI to Player 1
    asserta( player(2, computer, 4) ), % Assign Random AI to Player 2
    nl, write('play start'),
    play_clean(1),                         % Start game with Player 1
    nl, write('play end'),
    board(B),                        % Get final board state
    output_board(B),                 % Output final board
    write('Game over: '),
    output_winner(B),
    (   win(B, 'x') -> X1 is X + 1, O1 is O, D1 is D  % Player X wins
    ;   win(B, 'o') -> X1 is X, O1 is O + 1, D1 is D  % Player O wins
    ;   X1 is X, O1 is O, D1 is D + 1                 % Draw
    ),
    retract(board(_)),
    retract(player(_, _, _)),
    N1 is N - 1,
    nl, write('Games remaining: '), write(N1), nl,
    run_games(N1, X1, O1, D1, XFinal, OFinal, DFinal).

play_clean(P) :-
    board(B), !,
    (game_over(P, B) -> true ;  % Stop if game is over
        make_move(P, B), !,
        next_player(P, P2), !,
        play_clean(P2)).


% Entry point to start simulation with N games
simulate(N) :-
    nl, nl,
    run_simulation(N, XWins, OWins, Draws).



% Run multiple games between two Random AIs and track results
run_simulation(N, XWins, OWins, Draws) :-
    run_games(N, 0, 0, 0, XWins, OWins, Draws).

% Base case: No more games to play, return results
run_games(0, X, O, D, XFinal, OFinal, DFinal) :- !,
    write('Results:'), nl,
    write('Player X wins: '), write(X), nl,
    write('Player O wins: '), write(O), nl,
    write('Draws: '), write(D), nl.

% Play a game, update results, and recurse
run_games(N, X, O, D, XFinal, OFinal, DFinal) :-
    initialize,                     % Reset board for a new game
    asserta( player(1, computer, 1) ), % Assign Random AI to Player 1
    asserta( player(2, computer, 4) ), % Assign Random AI to Player 2
    nl, write('play start'),
    play_clean(1),                         % Start game with Player 1
    nl, write('play end'),
    board(B),                        % Get final board state
    output_board(B),                 % Output final board
    write('Game over: '),
    output_winner(B),
    (   win(B, 'x') -> X1 is X + 1, O1 is O, D1 is D  % Player X wins
    ;   win(B, 'o') -> X1 is X, O1 is O + 1, D1 is D  % Player O wins
    ;   X1 is X, O1 is O, D1 is D + 1                 % Draw
    ),
    retract(board(_)),
    retract(player(_, _, _)),
    N1 is N - 1,
    nl, write('Games remaining: '), write(N1), nl,
    run_games(N1, X1, O1, D1, XFinal, OFinal, DFinal).

play_clean(P) :-
    board(B), !,
    (game_over(P, B) -> true ;  % Stop if game is over
        make_move(P, B), !,
        next_player(P, P2), !,
        play_clean(P2)).


% Entry point to start simulation with N games
simulate(N) :-
    nl, nl,
    run_simulation(N, XWins, OWins, Draws).




:- run. % Start the game.