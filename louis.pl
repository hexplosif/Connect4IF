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
    write(' in square '), write(S), write('.').


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