blank_mark('e').        %%% the mark used in an empty square
inverse_mark('x','o').  %%% the inverse of x is o
inverse_mark('o','x').  %%% the inverse of o is x



%.......................................
% rectangle
%.......................................
% retrieves all the top values of the columns in a given board that are M
%

rectangle(B, Col, M) :- 
    nth1(Col, B, RowList),  % get the column
    nth1(6, RowList, M).    % checks if top of column is M

%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty top of columns) on a board.
%

moves(B,L) :-
    not(win(B,x)),                %%% if either player already won, then there are no available moves
    not(win(B,o)),
    blank_mark(E),
    findall(N, rectangle(B,N,E), L), 
    L \= []
    
%.......................................
% defensive_evaluation
%.......................................


transpose([], []).
transpose([[] | _], []).
transpose(Matrix, [Row | TransposedTail]) :-
    extract_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, TransposedTail).

extract_column([], [], []).
extract_column([[H | T] | Rows], [H | Column], [T | RestRows]) :-
    extract_column(Rows, Column, RestRows).

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
        combinationDanger(Comb, Opponent),
        write(Comb), nl
    ), Matches),
    length(Matches, Count),
    NewU is U - Count * 10.

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
        combinationDanger(Comb, Opponent),
        write(Comb), nl
    ), Matches),
    length(Matches, Count),
    NewU is U - Count * 10.

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

% Évaluation globale défensive
defensive_evaluation(Board, U, M) :-
    inverse_mark(M, Opponent),
    U2 = 0,
    defensive_horizontal_evaluation(Board, Opponent, U2, NewU1),
    defensive_vertical_evaluation(Board, Opponent, NewU1, NewU),
    defensive_diagonal_evaluation(Board, Opponent, NewU, U).


% Utility défensif
utility(B, U, M) :-
    win(B, 'x'),
    U = 1000000,
    !.

utility(B, U, M) :-
    win(B, 'o'),
    U = -1000000,
    !.

utility(B, U, M) :-
    defensive_evaluation(B, M, U).

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
    utility(B,U,M)      
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if max depth is reached
% if there is only one move left in the list...

best(5,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    utility(B2,U,M2),  %%% then search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(5,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    U1 = 0,
    U2 = 0,
    utility(B2,U1,M2),      %%% recursively search for the utility value of that move,
    best(5,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    U1 = 0,
    U2 = 0,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
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
% Tests de la fonction d'évaluation défensive
%.......................................

test_defensive_evaluation :-
    B = [['o', 'o', 'e', 'e', 'e', 'e'],
         ['e', 'e', 'e', 'e', 'e', 'e'],
         ['o', 'e', 'e', 'e', 'e', 'e'],
         ['x', 'o', 'e', 'e', 'e', 'e'],
         ['x', 'x', 'e', 'e', 'e', 'e'],
         ['e', 'e', 'e', 'e', 'e', 'e'],
         ['x', 'x', 'e', 'e', 'e', 'e']],
    defensive_evaluation(B, U, 'x'),
    write('Utility pour joueur X: '), write(U), nl,

    B2 = [['o', 'x', 'e', 'e', 'e', 'e'],
          ['o', 'o', 'e', 'e', 'e', 'e'],
          ['x', 'x', 'e', 'e', 'e', 'e'],
          ['x', 'e', 'e', 'e', 'e', 'e'],
          ['x', 'x', 'e', 'e', 'e', 'e'],
          ['o', 'e', 'e', 'e', 'e', 'e'],
          ['o', 'o', 'e', 'e', 'e', 'e']],
    defensive_evaluation(B2, U2, 'o'),
    write('Utility pour joueur O: '), write(U2), nl.

:- test_defensive_evaluation.
