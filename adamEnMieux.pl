%%%%%%%%%%%%%%%%%%%%%%
%%% IHM
%%%%%%%%%%%%%%%%%%%%%%

/*
Pour lancer le programme, éxécutez la commande:
swipl-win adamEnMieux.pl

Puis dans SWI-Prolog, tapez:
start.

Single letter variables represent:
B: Board
X: X coordinate
Y: Y coordinate
N: Number of circles
M: Marker (x, o, _)
W: Window

*/

:- use_module(library(pce)).

% Crée une fenêtre pour afficher le menu
create_menu_window :-
    new(W, dialog('Connect4IF')), % Crée une fenêtre pour le menu
    send(W, size, size(550, 300)),
    
    % Ajouter des cercles petits et nombreux de manière désordonnée
    add_colored_circles(W, 100),  % Ajouter 50 cercles aléatoires

    % Appliquer un fond sombre à la fenêtre
    send(W, background, colour(black)),  % Fond noir
    
    % Définir le titre avec une police plus grande et texte clair
    send(W, append, new(Titre, text('Connect4IF'))),
    send(Titre, font, font(helvetica, bold, 26)),  % Changer la taille du texte
    send(Titre, colour, colour(white)),  % Texte en blanc

    % Ajouter un espace entre le titre et les boutons
    send(W, append, new(_, text('\n\n\n'))),
    
    % Ajouter les boutons avec un style sombre
    send(W, append, new(Button1, button('Play against a human', message(@prolog, start_game, human)))),
    send(W, append, new(Button2, button('Play against an AI', message(@prolog, start_game, ai)))),
    send(W, append, new(Button3, button('AI against itself', message(@prolog, start_game, ia_vs_ia)))),
    send(W, append, new(Button4, button('Quit', message(W, destroy)))),

    % Ouvrir la fenêtre
    send(W, open).

% Ajouter des cercles alternant entre rouge et bleu
add_colored_circles(W, N) :-
    add_colored_circles(W, N, 0).

add_colored_circles(_, 0, _) :- !.  % Arrêter quand N atteint 0
add_colored_circles(W, N, Counter) :-
    RandomX is random(550),  % Générer une position X aléatoire
    RandomY is random(300),  % Générer une position Y aléatoire
    send(W, display, new(Circle, circle(10))),  % Créer un cercle de rayon 10
    
    % Alterner les couleurs entre rouge et bleu
    (   0 is Counter mod 2
    ->  send(Circle, fill_pattern, colour(red))  % Rouge pour les cercles pairs
    ;   send(Circle, fill_pattern, colour(blue))  % Bleu pour les cercles impairs
    ),
    
    send(Circle, move, point(RandomX, RandomY)),  % Déplacer à la position aléatoire
    NewCounter is Counter + 1,
    NewN is N - 1,
    add_colored_circles(W, NewN, NewCounter).  % Rappel pour ajouter le prochain cercle

% Crée une fenêtre pour afficher le plateau
create_board_window(B) :-
    new(W, picture('Connect4IF')),
    send(W, size, size(720, 700)),
    send(W, open),
    send(W, background, colour(black)),
    draw_board(W, B, 6, 7).

% Démarre le jeu selon le mode sélectionné
start_game(Mode) :-
    format('Mode choisi: ~w~n', [Mode]),
    B = [[x, o, _, _, _, _],
         [x, x, _, _, _, _],
         [o, o, _, _, _, _],
         [_, _, _, _, _, _],
         [o, _, _, _, _, _],
         [x, _, _, _, _, _],
         [_, _, _, _, _, _]],
    create_board_window(B).

% Dessine le plateau sur la fenêtre
draw_board(W, B, Rows, Cols) :-
    draw_cells(W, B, Rows, Cols, Rows, 1),
    draw_buttons(W, Cols).

% Dessine les boutons pour chaque colonne, sous le plateau
draw_buttons(W, Cols) :-
    ButtonY is 100 * 6 + 30, % Position sous les cellules
    draw_buttons(W, 1, Cols, ButtonY).

draw_buttons(_, Col, TotalCols, _) :-
    Col > TotalCols, !.
draw_buttons(W, Col, TotalCols, ButtonY) :-
    X is (Col - 1) * 100 + 30,
    new(Button, button(Col, message(@prolog, play_move, W, Col))),
    send(Button, colour, colour(white)),
    send(W, display, Button, point(X, ButtonY)),
    NextCol is Col + 1,
    draw_buttons(W, NextCol, TotalCols, ButtonY).

% Gestion du jeu : un coup dans une colonne
play_move(W, Col) :-
    format('Colonne choisie: ~w~n', [Col]),
    update_board(W, Col).

% Met à jour le plateau et le réaffiche
update_board(W, Col) :-
    B = [[x, o, _, _, _, _],
         [x, x, _, _, _, _],
         [o, o, _, _, _, _],
         [_, _, _, _, _, _],
         [o, o, _, _, _, _],
         [x, _, _, _, _, _],
         [_, _, _, _, _, _]],
    send(W, clear), % Efface le contenu précédent de la fenêtre
    draw_board(W, B, 6, 7).

% Parcourt les cellules pour les dessiner
draw_cells(_, _, 0, _, _, _) :- !.
draw_cells(W, B, Row, Cols, TotalRows, TotalCols) :-
    draw_row(W, B, Row, Cols, TotalRows, TotalCols),
    NextRow is Row - 1,
    draw_cells(W, B, NextRow, Cols, TotalRows, TotalCols).

% Dessine une ligne donnée
draw_row(_, _, _, 0, _, _) :- !.
draw_row(W, B, Row, Col, TotalRows, TotalCols) :-
    nth1(Col, B, Column),
    nth1(Row, Column, M),
    CellY is (TotalRows - Row) * 100 + 30,
    CellX is (Col - 1) * 100 + 30,
    draw_cell(W, M, CellX, CellY),
    NextCol is Col - 1,
    draw_row(W, B, Row, NextCol, TotalRows, TotalCols).

% Dessine une cellule donnée
draw_cell(W, M, X, Y) :-
    (   M == x
    ->  new(Circle, circle(80)),
        send(Circle, fill_pattern, colour(red)),
        send(W, display, Circle, point(X, Y))
    ;   M == o
    ->  new(Circle, circle(80)),
        send(Circle, fill_pattern, colour(blue)),
        send(W, display, Circle, point(X, Y))
    ;   new(Box, box(80, 80)),
        send(Box, fill_pattern, colour(grey20)),
        send(W, display, Box, point(X, Y))
    ).

% Exemple d'appel
start :-
    create_menu_window.
