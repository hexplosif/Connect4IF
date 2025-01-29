% Assign weights to each row (6 rows in total)
center_weight(1, [1, 2, 3, 4, 3, 2, 1]).
center_weight(2, [2, 3, 4, 5, 4, 3, 2]).
center_weight(3, [3, 4, 5, 6, 5, 4, 3]).
center_weight(4, [3, 4, 5, 6, 5, 4, 3]).
center_weight(5, [2, 3, 4, 5, 4, 3, 2]).
center_weight(6, [1, 2, 3, 4, 3, 2, 1]).

% Evaluate board position based on weighted center strategy
evaluate_board(Board, Player, Score) :-
    findall(W, (nth1(Row, Board, Line), center_weight(Row, Weights), 
                nth1(Col, Line, Player), nth1(Col, Weights, W)), Scores),
    sum_list(Scores, Score).