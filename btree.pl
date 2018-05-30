:- use_module(library(clpfd)).

% https://sites.google.com/site/prologsite/prolog-problems/4

% 4.01
istree(nil).
istree(t(_,L,R)) :- istree(L), istree(R).

% 4.02
cbal_tree(X, nil) :- X #= 0.
cbal_tree(X, t(_,L,R)) :-
    X #> 0,
    SL #< X, cbal_tree(SL, L),
    SR #< X, cbal_tree(SR, R),
    SL + SR #= X - 1,
    ( SL #= SR
    ; SL #= SR + 1
    ; SR #= SL + 1
    ).

% 4.03
symmetric(nil).
symmetric(t(_,L,R)) :- mirror_tree(L, R).

mirror_tree(nil, nil).
mirror_tree(t(_,LL, LR), t(_, RL, RR)) :-
    mirror_tree(LR, RL),
    mirror_tree(LL, RR).