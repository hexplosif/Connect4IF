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

win(B, M) :- 
    horizontal_win(B, M);
    vertical_win(B, M).
