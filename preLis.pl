preList([], L).
preList([H_s|T_s], [H_s|Tail]):-
    preList(T_s, Tail).

subList([H_s|T_s], [H_s|Tail]):-
    preList(T_s, Tail).

subList([H_s|T_s], [H_s|Tail]):-
    subList([H_s|T_s], Tail).
    
subList(Sub, [_|Tail]):-
    subList(Sub, Tail).

isSubList
