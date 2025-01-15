:- use_module(library(pce)).

%%%%%%%%%%%%%%%%%%%%%%
%%% Affichage Terminal (IHM Texte)
%%%%%%%%%%%%%%%%%%%%%%

% Affiche le plateau de jeu dans le terminal
output_board_terminal(Board) :-
    nl,
    output_rows(Board, 6), % On commence par la 6ème ligne (le bas) sinon ça s'affiche à l'envert
    output_column_numbers, % Affiche les numéros de colonnes
    nl.

% Affiche les lignes du bas vers le haut
output_rows(_, 0) :- !.
output_rows(Board, Row) :-
    output_row(Board, Row),
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

% Affiche les numéros des colonnes (1 à 7)
output_column_numbers :-
    write('---------------'), nl,
    write(' 1 2 3 4 5 6 7'), nl.


%%%%%%%%%%%%%%%%%%%%%%
%%% Affichage Graphique (XPCE)
%%%%%%%%%%%%%%%%%%%%%%

% Affiche le plateau de jeu dans une fenêtre graphique
output_board_graphical(Board) :-
    new(Window, window('Plateau de jeu', size(400, 300))),
    send(Window, open),
    create_board(Window, Board),
    !.

% Crée une grille pour le plateau
create_board(Window, Board) :-
    % Crée un objet 'canvas' pour dessiner le plateau
    new(Canvas, canvas),
    send(Window, display, Canvas),
    
    % Dimension de la grille
    NumRows = 6, % 6 lignes
    NumCols = 7, % 7 colonnes

    % Affiche les cellules du plateau
    draw_cells(Canvas, Board, NumRows, NumCols).

% Dessine les cellules du plateau dans le canvas
draw_cells(Canvas, Board, NumRows, NumCols) :-
    RowHeight = 30, % Hauteur des cellules
    ColWidth = 50,  % Largeur des cellules
    Top = 10, Left = 10,
    draw_rows(Canvas, Board, 1, NumRows, 1, NumCols, RowHeight, ColWidth, Top, Left).

% Dessine chaque ligne du plateau
draw_rows(_, _, Row, NumRows, _, _, _, _, _, _) :-
    Row > NumRows, !.
draw_rows(Canvas, Board, Row, NumRows, Col, NumCols, RowHeight, ColWidth, Top, Left) :-
    draw_columns(Canvas, Board, Row, Col, NumCols, RowHeight, ColWidth, Top, Left),
    NextRow is Row + 1,
    draw_rows(Canvas, Board, NextRow, NumRows, Col, NumCols, RowHeight, ColWidth, Top, Left).

% Dessine chaque colonne du plateau
draw_columns(_, _, _, Col, NumCols, _, _, _, _) :-
    Col > NumCols, !.
draw_columns(Canvas, Board, Row, Col, NumCols, RowHeight, ColWidth, Top, Left) :-
    % Calcul de la position de la cellule
    X is Left + (Col - 1) * ColWidth,
    Y is Top + (Row - 1) * RowHeight,

    % Récupère la valeur de la cellule
    nth1(Col, Board, Column),
    nth1(Row, Column, Cell),

    % Dessine le carré
    draw_square(Canvas, X, Y, ColWidth, RowHeight, Cell),
    
    NextCol is Col + 1,
    draw_columns(Canvas, Board, Row, NextCol, NumCols, RowHeight, ColWidth, Top, Left).

% Dessine un carré pour une cellule donnée
draw_square(Canvas, X, Y, ColWidth, RowHeight, Cell) :-
    (var(Cell) -> Color = white ; (Cell == x -> Color = red ; Color = blue)),
    new(Rect, rectangle(X, Y, ColWidth, RowHeight)),
    send(Rect, fill_color, Color),
    send(Canvas, display, Rect).

% Exemple d'appel à output_board
example :-
    Board = [[x, o, _, _, _, _], 
             [x, x, _, _, _, _], 
             [o, o, _, _, _, _], 
             [_, _, _, _, _, _], 
             [o, _, _, _, _, _], 
             [x, _, _, _, _, _], 
             [_, _, _, _, _, _]],
    output_board_terminal(Board), % Affiche dans le terminal
    output_board_graphical(Board). % Affiche graphiquement