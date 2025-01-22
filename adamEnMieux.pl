%%%%%%%%%%%%%%%%%%%%%%
%%% IHM
%%%%%%%%%%%%%%%%%%%%%%

/*
Pour lancer le programme, éxécutez la commande:
swipl-win adamEnMieux.pl

Puis dans SWI-Prolog, tapez:
start.
*/

:- use_module(library(pce)).

% Crée une fenêtre pour afficher le menu
create_menu_window :-
    new(Menu, dialog('Connect4IF')), % Crée une fenêtre pour le menu
    send(Menu, size, size(550, 300)),
    
    % Ajouter des cercles petits et nombreux de manière désordonnée
    add_colored_circles(Menu, 100),  % Ajouter 50 cercles aléatoires

    % Appliquer un fond sombre à la fenêtre
    send(Menu, background, colour(black)),  % Fond noir
    
    % Définir le titre avec une police plus grande et texte clair
    send(Menu, append, new(Titre, text('Connect4IF'))),
    send(Titre, font, font(helvetica, bold, 26)),  % Changer la taille du texte
    send(Titre, colour, colour(white)),  % Texte en blanc

    % Ajouter un espace entre le titre et les boutons
    send(Menu, append, new(_, text('\n\n\n'))),
    
    % Ajouter les boutons avec un style sombre
    send(Menu, append, new(Button1, button('Play against a human', message(@prolog, start_game, human)))),
    send(Menu, append, new(Button2, button('Play against an AI', message(@prolog, start_game, ai)))),
    send(Menu, append, new(Button3, button('AI against itself', message(@prolog, start_game, ia_vs_ia)))),
    send(Menu, append, new(Button4, button('Quit', message(Menu, destroy)))),

    % Ouvrir la fenêtre
    send(Menu, open).

% Ajouter des cercles alternant entre rouge et bleu
add_colored_circles(Menu, N) :-
    add_colored_circles(Menu, N, 0).

add_colored_circles(_, 0, _) :- !.  % Arrêter quand N atteint 0
add_colored_circles(Menu, N, Counter) :-
    RandomX is random(550),  % Générer une position X aléatoire
    RandomY is random(300),  % Générer une position Y aléatoire
    send(Menu, display, new(Circle, circle(10))),  % Créer un cercle de rayon 10
    
    % Alterner les couleurs entre rouge et bleu
    (   0 is Counter mod 2
    ->  send(Circle, fill_pattern, colour(red))  % Rouge pour les cercles pairs
    ;   send(Circle, fill_pattern, colour(blue))  % Bleu pour les cercles impairs
    ),
    
    send(Circle, move, point(RandomX, RandomY)),  % Déplacer à la position aléatoire
    NewCounter is Counter + 1,
    NewN is N - 1,
    add_colored_circles(Menu, NewN, NewCounter).  % Rappel pour ajouter le prochain cercle






% Crée une fenêtre pour afficher le plateau
create_board_window(Board) :-
    new(Window, picture('Connect4IF')),
    send(Window, size, size(720, 700)),
    send(Window, open),
    send(Window, background, colour(black)),
    draw_board(Window, Board, 6, 7).

% Démarre le jeu selon le mode sélectionné
start_game(Mode) :-
    format('Mode choisi: ~w~n', [Mode]),
    Board = [[x, o, _, _, _, _],
             [x, x, _, _, _, _],
             [o, o, _, _, _, _],
             [_, _, _, _, _, _],
             [o, _, _, _, _, _],
             [x, _, _, _, _, _],
             [_, _, _, _, _, _]],
    create_board_window(Board).

% Dessine le plateau sur la fenêtre
draw_board(Window, Board, Rows, Cols) :-
    draw_cells(Window, Board, Rows, Cols, Rows, 1),
    draw_buttons(Window, Cols).

% Dessine les boutons pour chaque colonne, sous le plateau
draw_buttons(Window, Cols) :-
    ButtonY is 100 * 6 + 30, % Position sous les cellules
    draw_buttons(Window, 1, Cols, ButtonY).

draw_buttons(_, Col, TotalCols, _) :-
    Col > TotalCols, !.
draw_buttons(Window, Col, TotalCols, ButtonY) :-
    X is (Col - 1) * 100 + 30,
    new(Button, button(Col, message(@prolog, play_move, Window, Col))),
    send(Button, colour, colour(white)),
    send(Window, display, Button, point(X, ButtonY)),
    NextCol is Col + 1,
    draw_buttons(Window, NextCol, TotalCols, ButtonY).

% Gestion du jeu : un coup dans une colonne
play_move(Window, Col) :-
    format('Colonne choisie: ~w~n', [Col]),
    update_board(Window, Col).

% Met à jour le plateau et le réaffiche
update_board(Window, Col) :-
    Board = [[x, o, _, _, _, _],
             [x, x, _, _, _, _],
             [o, o, _, _, _, _],
             [_, _, _, _, _, _],
             [o, o, _, _, _, _],
             [x, _, _, _, _, _],
             [_, _, _, _, _, _]],
    send(Window, clear), % Efface le contenu précédent de la fenêtre
    draw_board(Window, Board, 6, 7).

% Parcourt les cellules pour les dessiner
draw_cells(_, _, 0, _, _, _) :- !.
draw_cells(Window, Board, Row, Cols, TotalRows, TotalCols) :-
    draw_row(Window, Board, Row, Cols, TotalRows, TotalCols),
    NextRow is Row - 1,
    draw_cells(Window, Board, NextRow, Cols, TotalRows, TotalCols).

% Dessine une ligne donnée
draw_row(_, _, _, 0, _, _) :- !.
draw_row(Window, Board, Row, Col, TotalRows, TotalCols) :-
    nth1(Col, Board, Column),
    nth1(Row, Column, Cell),
    CellY is (TotalRows - Row) * 100 + 30,
    CellX is (Col - 1) * 100 + 30,
    draw_cell(Window, Cell, CellX, CellY),
    NextCol is Col - 1,
    draw_row(Window, Board, Row, NextCol, TotalRows, TotalCols).

% Dessine une cellule donnée
draw_cell(Window, Cell, X, Y) :-
    (   Cell == x
    ->  new(Circle, circle(80)),
        send(Circle, fill_pattern, colour(red)),
        send(Window, display, Circle, point(X, Y))
    ;   Cell == o
    ->  new(Circle, circle(80)),
        send(Circle, fill_pattern, colour(blue)),
        send(Window, display, Circle, point(X, Y))
    ;   new(Box, box(80, 80)),
        send(Box, fill_pattern, colour(grey20)),
        send(Window, display, Box, point(X, Y))
    ).

% Exemple d'appel
start :-
    create_menu_window.
