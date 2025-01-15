asserta( board([[E, E, E, E, E, E], 
                [E, E, E, E, E, E], 
                [E, E, E, E, E, E], 
                [E, E, E, E, E, E], 
                [E, E, E, E, E, E], 
                [E, E, E, E, E, E], 
                [E, E, E, E, E, E]]) )  %%% create a blank board

isconsecutive()
transpose()

horizontal_win(Board, M) :- 
    member(Row, Board),
    isconsecutive(Row, M).

vertical_win(Board, M) :- 
    transpose(Board, TBoard),
    member(Row, TBoard),
    isconsecutive(Row, M).

win(B, M) :- 
    horizontal_win(Board, M);
    vertical_win(Board, M).
    diagonal_win(Board, M).
