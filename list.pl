:- use_module(library(clpfd)).

head_list(H, [H|_]).

last_list(L, [L]).
last_list(L, [_|Ls]) :- last_list(L, Ls).

tail_list(T, [_|T]).

init_list([], [_]).
init_list([X|Xs], [X|Y]) :- init_list(Xs, Y).

head_tail_list(H, T, [H|T]).

null([]).

list_length([], 0).
list_length([_|Ls],N1) :-
    N1 #> 0,
    N1 #= N + 1,
    list_length(Ls, N).

reverse([], []).
reverse([X|Y], Z) :-
    last_list(X, Z),
    init_list(C, Z),
    reverse(Y, C).

concat([], X, X).
concat([X|Xs], Y, [Z|Zs]) :-
    X #= Z,
    concat(Xs, Y, Zs).

intersperse_elem_list(_, [], []).
intersperse_elem_list(_, [X], [X]).
intersperse_elem_list(E, [X,Y|Z], [X,E|T]) :-
    intersperse_elem_list(E, [Y|Z], T).

all_list(+_, []).
all_list(+Goal, [X|Xs]) :-
    call(Goal, X),
    all_list(Goal, Xs).

any_list(+Goal, [X|Xs]) :-
    call(Goal, X);
    any_list(Goal, Xs).

zip_list([], [], _).
zip_list([], _, []).
zip_list([(X,Y)|Ps], [X|Xs], [Y|Ys]) :-
    zip_list(Ps, Xs, Ys).

maximum_list(M, [M]).
maximum_list(M, [X|Xs]) :-
    maximum_list(Ms, Xs),
    ( X #>= Ms, M #= X
    ; X #<  Ms, M #= Ms
    ).

minimum_list(M, [M]).
minimum_list(M, [X|Xs]) :-
    minimum_list(Ms, Xs),
    ( X #=< Ms, M #= X
    ; X #>  Ms, M #= Ms
    ).

elem_list(E, [X|Xs]) :- E #= X; elem_list(E, Xs).

map_list(_, [], []).
map_list(Rel, [X|Xs], [Y|Ys]) :-
    call(Rel, X, Y),
    map_list(Rel, Xs, Ys).