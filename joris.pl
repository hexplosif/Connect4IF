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
    .

%.......................................
% horizontal and vertical evaluation
%.......................................
% tools necessary to determine the evaluation of a given board position
%


transpose([], []).
transpose([[] | _], []).
transpose(Matrix, [Row | TransposedTail]) :-
    extract_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, TransposedTail).

extract_column([], [], []).
extract_column([[H | T] | Rows], [H | Column], [T | RestRows]) :-
    extract_column(Rows, Column, RestRows).

horizontal_evaluation(Board, Comb, U, NewU) :- 
    findall(_, (member(Row, Board), append([_, Comb, _], Row)), Matches),
    length(Matches, Count),
    NewU is U + Count,
    true
    .

vertical_evaluation(Board, Comb, U, NewU) :- 
    transpose(Board, TBoard),
    findall(_, (member(Row, TBoard), append([_, Comb, _], Row)), Matches),
    length(Matches, Count),
    NewU is U + Count,
    true
    .

diagonal_evaluation(Board, Comb, U, NewU) :- 
    diagonals(Board, Diags),
    findall(_, (member(Diag, Diags), append([_, Comb, _], Diag)), Matches),
    length(Matches, Count),
    NewU is U + Count,
    true
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

%.......................................
% evaluation
%.......................................
% determines the value of a given board position
%
combinationX([ ['x','x','x'], 
                ['x','x'] ]).

combinationO([ ['o','o','o'], 
                ['o','o'] ]).

evaluate([], B).

evaluate([H|T],B) :-
    valeurU(U1),
    horizontal_evaluation(B,H,U1,NewU1),
    vertical_evaluation(B,H,NewU1,NewU2),
    diagonal_evaluation(B,H,NewU2,NewU3),
    retract(valeurU(_)),
    asserta(valeurU(NewU3)),
    evaluate(T, B)
    .

%.......................................
% utility
%.......................................
% determines the value of a given board position
%

/*
utility(B,U,M) :-
    win(B,'x'),
    U = 1000000, 
    !
    .

utility(B,U,M) :-
    win(B,'o'),
    U = (-1000000), 
    !
    .
*/

utility(B,U,'x') :-
    asserta(valeurU(0)),
    retract(valeurU(_)),
    asserta(valeurU(0)),
    combinationX(C),
    evaluate(C,B),
    valeurU(U1),
    U = U1.

utility(B,U,'o') :-
    asserta(valeurU(0)),
    retract(valeurU(_)),
    asserta(valeurU(0)),
    combinationO(C),
    evaluate(C,B),
    valeurU(U1),
    U = -U1.

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


test_evaluate :-
    B = [['o', 'o', 'e', 'e', 'e', 'e'],
         ['e', 'e', 'e', 'e', 'e', 'e'],
         ['o', 'e', 'e', 'e', 'e', 'e'],
         ['x', 'o', 'e', 'e', 'e', 'e'],
         ['x', 'x', 'e', 'e', 'e', 'e'],
         ['e', 'e', 'e', 'e', 'e', 'e'],
         ['x', 'x', 'e', 'e', 'e', 'e']],
    utility(B,U,'x'),
    write('Utility pour joueur X: '), write(U), nl,

    B2 = [['o', 'e', 'e', 'e', 'e', 'e'],
          ['x', 'o', 'e', 'e', 'e', 'e'],
          ['e', 'e', 'e', 'e', 'e', 'e'],
          ['e', 'e', 'e', 'e', 'e', 'e'],
          ['e', 'e', 'e', 'e', 'e', 'e'],
          ['e', 'e', 'e', 'e', 'e', 'e'],
          ['x', 'e', 'e', 'e', 'e', 'e']],
    utility(B2,U2,'o'),
    write('Utility pour joueur O: '), write(U2), nl.

:- test_evaluate.