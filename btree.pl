:- use_module(library(clpfd)).

% https://sites.google.com/site/prologsite/prolog-problems/4

% 4.01
istree(nil).
istree(t(_,L,R)) :- istree(L), istree(R).

% 4.02
% TODO get rid of false at the end
cbal_tree(0,nil) :- !.
cbal_tree(X,t(_,L,R)) :-
    X #> 1, X1 #= X - 1,
    H1 #= X1 // 2,
    H2 #= X1 - H1,
    ( H2 #= H1 + 1
    , ( SL #= H1, SR #= H2
      ; SL #= H2, SR #= H1
      )
    ; H2 #= H1, SL #= H1, SR #= H1
    ),
    cbal_tree(SL, L),
    cbal_tree(SR, R).

cbal_tree2(0,nil) :- !.
cbal_tree2(N,t(x,L,R)) :- N > 0,
	N0 is N - 1, 
	N1 is N0//2, N2 is N0 - N1,
	distrib(N1,N2,NL,NR),
    cbal_tree2(NL,L),
    cbal_tree2(NR,R).

distrib(N,N,N,N) :- !.
distrib(N1,N2,N1,N2).
distrib(N1,N2,N2,N1).

% 4.03
symmetric(nil).
symmetric(t(_,L,R)) :- mirror_tree(L, R).

mirror_tree(nil, nil).
mirror_tree(t(_,LL, LR), t(_, RL, RR)) :-
    mirror_tree(LR, RL),
    mirror_tree(LL, RR).

% 4.04
% TODO

% 4.05
% TODO

% 4.06
hbal_tree(0, nil) :- !.
hbal_tree(1, t(_,nil,nil)) :- !.
hbal_tree(H, t(_,L,R)) :-
    H #> 1, H1 #= H - 1,
    height_children(H1,HL,HR),
    hbal_tree(HL,L),
    hbal_tree(HR,R).

height_children(H,H,H).
height_children(H,H1,H) :- H1 #= H-1.
height_children(H,H,H1) :- H1 #= H-1.

% 4.07
% minNodes(H,N) :- N is the minimum number of nodes in a height-balanced binary tree of height H.
minNodes(0,0).
minNodes(1,1).
minNodes(2,4).
minNodes(H,N) :- H #> 2,
    H1 #= H-1, minNodes(H1,N1),
    H2 #= H-2, minNodes(H2,N2),
    N is N1+N2+1.
