# Connect4IF

Projet ALIA

Membres du groupe projet :
BONKOUNGOU Mathis
FELZINES Joris
KUSNO Louis
MANTZARIDES Guillaume
SCHLEE Adam
STEPHAN Justine

## Run simulations

Chose the AI models by changing line 902 and 903 to the AI id. Example : makes player 1 use MinMax offensive:
```
asserta( player(1, computer, 3) ),
```

Then reload the program and run this command to simulate 100 games :
```
simulate(100).
```

Only the board when the game ends is shown, so it might not seam like it is running at first


## Main functions:

run: pour lancer le programme. Initialise le plateau, démarre une partie. (fonction à appeler par l'utilisateur pour jouer).  
initialize : crée un plateau vide  
set_players  
player  
make_move  
move  
set_item  
output_board  
random_int_1n : returns a random integer from 1 to N  


