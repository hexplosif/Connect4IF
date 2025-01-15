% -------------
% Data representation
% -------------


% retract(predicat(_)) retire la ligne de prédicat si elle existe
% asserta(predicat(B)) rajoute la ligne predicat(B) dans le code
% à partir de ce moment là predicat(B) permet de stocker dans B la valeur du prédicat
Board ([[e,e,e,e,e,e],
     [e,e,e,e,e,e],
     [e,e,e,e,e,e],
     [e,e,e,e,e,e],
     [e,e,e,e,e,e],
     [e,e,e,e,e,e],
     [e,e,e,e,e,e]])


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position