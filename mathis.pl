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

win(B, M) :- 
    horizontal_win(B, M);
    vertical_win(B, M).
    diagonal_win_check(Board, M).

% run :-
%     M1 = ['x','x','x','x'],
%     append([_, M1, _], ['x','x','x','x','e','x','x']),
%     write(M1).
