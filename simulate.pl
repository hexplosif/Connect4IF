%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).    
next_player(2, 1).

player_mark(1, 'x').    
player_mark(2, 'o').    

opponent_mark(1, 'o').  
opponent_mark(2, 'x').

blank_mark('e').

maximizing('x').        
minimizing('o').     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     BOARD INITIALIZATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     AI MODELS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_ai(Board, Move) :-
    moves(Board, AvailableMoves),
    random_member(Move, AvailableMoves).

minimax_ai(Board, Depth, Player, BestMove) :-
    findall(Score-Move, (moves(Board, Moves), member(Move, Moves),
                        move(Board, Move, Player, NewBoard),
                        minimax(NewBoard, Depth, Player, Score)), ScoredMoves),
    best_move(ScoredMoves, BestMove).

minimax(Board, 0, Player, Score) :- evaluate_board(Board, Player, Score), !.
minimax(Board, Depth, Player, Score) :-
    Depth > 0,
    next_player(Player, Opponent),
    findall(S, (moves(Board, Moves), member(Move, Moves),
                move(Board, Move, Player, NewBoard),
                NewDepth is Depth - 1,
                minimax(NewBoard, NewDepth, Opponent, S)), Scores),
    best_score(Scores, Player, Score).

best_score(Scores, Player, BestScore) :-
    (maximizing(Player) -> max_list(Scores, BestScore) ;
     minimizing(Player) -> min_list(Scores, BestScore)).

best_move(ScoredMoves, BestMove) :-
    sort(ScoredMoves, Sorted),
    (Sorted = [BestScore-Move | _] -> BestMove = Move ; BestMove = none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MOVE MECHANICS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(Board, Col, Mark, NewBoard) :-
    drop_piece(Board, Col, Mark, NewBoard).

drop_piece([Col|Rest], 1, Mark, [NewCol|Rest]) :-
    drop_in_column(Col, Mark, NewCol).
drop_piece([Col|Rest], ColNum, Mark, [Col|NewRest]) :-
    ColNum > 1, 
    N is ColNum - 1, 
    drop_piece(Rest, N, Mark, NewRest).

drop_in_column([E|Rest], Mark, [Mark|Rest]) :- blank_mark(E), !.
drop_in_column([X|Rest], Mark, [X|NewRest]) :-
    drop_in_column(Rest, Mark, NewRest).

moves(Board, AvailableMoves) :-
    findall(N, (nth1(N, Board, Col), member('e', Col)), AvailableMoves).

make_move(Player, Board) :-
    (Player = 1 -> random_ai(Board, Move) ; minimax_ai(Board, 3, Player, Move)),
    move(Board, Move, Player, NewBoard),
    retract(board(Board)),   
    asserta(board(NewBoard)). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     EVALUATION FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate_board(Board, Player, Score) :-
    player_mark(Player, Mark),
    opponent_mark(Player, OppMark),
    count_pieces(Board, Mark, PlayerCount),
    count_pieces(Board, OppMark, OpponentCount),
    Score is PlayerCount - OpponentCount.

count_pieces(Board, Mark, Count) :-
    flatten(Board, Flat),
    include(==(Mark), Flat, Pieces),
    length(Pieces, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     AI VS AI SIMULATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_games(N, Results) :-
    simulate_games(N, 0, 0, 0, Results).

simulate_games(0, XWins, OWins, Draws, results(XWins, OWins, Draws)) :- 
    nl, write('Simulation complete.'), nl,
    write('X Wins: '), write(XWins), nl,
    write('O Wins: '), write(OWins), nl,
    write('Draws: '), write(Draws), nl.

simulate_games(N, XWins, OWins, Draws, Results) :-
    N > 0,
    initialize,                 
    play_ai_vs_ai(Winner),      
    update_results(Winner, XWins, OWins, Draws, NX, NO, ND), 
    N1 is N - 1,                
    simulate_games(N1, NX, NO, ND, Results).  

update_results(1, XWins, OWins, Draws, NX, OWins, Draws) :- NX is XWins + 1.
update_results(2, XWins, OWins, Draws, XWins, NO, Draws) :- NO is OWins + 1.
update_results(draw, XWins, OWins, Draws, XWins, OWins, ND) :- ND is Draws + 1.

play_ai_vs_ai(Winner) :-
    play_ai(1, Winner).

play_ai(P, Winner) :-
    board(Board),
    \+ game_over(Board),
    make_move(P, Board),
    next_player(P, NextPlayer),
    play_ai(NextPlayer, Winner).             

play_ai(_, Winner) :-
    board(B), game_over(_, B), determine_winner(B, Winner).

determine_winner(Board, 1) :- win(Board, 'x'), !.
determine_winner(Board, 2) :- win(Board, 'o'), !.
determine_winner(_, draw).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     WINNING CONDITION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game_over(Board) :-
    win(Board, 'x');
    win(Board, 'o');
    \+ (moves(Board, _)).

win(B, M) :- 
    horizontal_win(B, M);
    vertical_win(B, M);
    diagonal_win_check(B, M).

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

horizontal_win(Board, M) :- 
    member(Row, Board),
    isconsecutive(Row, M, 4).

vertical_win(Board, M) :- 
    transpose(Board, TBoard),
    member(Row, TBoard),
    isconsecutive(Row, M, 4).

diagonal_win_check(Board, M) :-
    length(Board, Rows),
    length(Board, Cols),
    RowStartMax is Rows - 3,
    ColStartMax is Cols - 3,
    between(0, RowStartMax, RowStart),
    between(0, ColStartMax, ColStart),
    diagonal_win_from(Board, RowStart, ColStart, M).

diagonal_win_check(Board, M) :-
    length(Board, Rows),
    length(Board, Cols),
    RowStartMax is Rows - 3,
    ColStartMax is Cols - 3,
    between(0, RowStartMax, RowStart),
    ColEndMax is Cols - 1,
    between(0, ColEndMax, ColEnd),
    diagonal_win_from_reverse(Board, RowStart, ColEnd, M).

diagonal_win_from(Board, RowStart, ColStart, M) :-
    nth0(RowStart, Board, Row1),
    nth0(ColStart, Row1, M),
    Row2Start is RowStart + 1, Col2Start is ColStart + 1,
    nth0(Row2Start, Board, Row2),
    nth0(Col2Start, Row2, M),
    Row3Start is Row2Start + 1, Col3Start is Col2Start + 1,
    nth0(Row3Start, Board, Row3),
    nth0(Col3Start, Row3, M),
    Row4Start is Row3Start + 1, Col4Start is Col3Start + 1,
    nth0(Row4Start, Board, Row4),
    nth0(Col4Start, Row4, M).

diagonal_win_from_reverse(Board, RowStart, ColEnd, M) :-
    nth0(RowStart, Board, Row1),
    nth0(ColEnd, Row1, M),
    Row2Start is RowStart + 1, Col2Start is ColEnd - 1,
    nth0(Row2Start, Board, Row2),
    nth0(Col2Start, Row2, M),
    Row3Start is Row2Start + 1, Col3Start is Col2Start - 1,
    nth0(Row3Start, Board, Row3),
    nth0(Col3Start, Row3, M),
    Row4Start is Row3Start + 1, Col4Start is Col3Start - 1,
    nth0(Row4Start, Board, Row4),
    nth0(Col4Start, Row4, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     RUN SIMULATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_simulation(N) :-
    simulate_games(N, Results),
    write(Results), nl.
