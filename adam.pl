%%%%%%%%%%%%%%%%%%%%%%
%%% IHM
%%%%%%%%%%%%%%%%%%%%%%

/* 
nth1(i, liste, element)
Ça met dans élément la ième valeur de la liste
*/

% Affiche le plateau de jeu
output_board(Board) :-
    nl,
    output_rows(Board, 6), % On commence par la 6ème ligne (le bas)
    nl.

% Affiche les lignes du bas vers le haut
output_rows(_, 0) :- !.
output_rows(Board, Row) :-
    output_row(Board, Row),
    nl,
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
output_square(Cell) :-
    (var(Cell) -> write(' '); write(Cell)), !. % Affiche un espace si la cellule est vide, sinon affiche le contenu (X, O, etc.)