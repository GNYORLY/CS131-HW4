
% official transpose/2 implementation from SWI-Prolog library transpose.pl
transpose([], L) :-
    once(maplist(=([]), L)).
transpose([C|Cs], L) :-
    deal_column(C, L, R),
    transpose(Cs, R).

deal_column([], L, L) :-
    once(maplist(=([]), L)).
deal_column([E|Es], [[E|R1]|L], [R1|R]) :-
    deal_column(Es, L, R).


% tower/3 uses the finite domain solver to find valid tower puzzles

fd_domain2(Min, Max, L) :- fd_domain(L, Min, Max).
length2(N, L) :- length(L, N).

tower(N,T,C) :-
length2(N,T),
maplist(length2(N), T),
maplist(fd_domain2(1,N), T),
transpose(T, Cols),
maplist(fd_all_different, T),
maplist(fd_all_different, Cols),
maplist(fd_labeling, T),
maplist(reverse, Cols, Col2),
maplist(reverse, T, T2),
maplist(vis, Cols,Top),
maplist(vis, Col2,Bot),
maplist(vis, T, Left),
maplist(vis, T2, Right),
C=counts(Top,Bot,Left,Right).

% vis/2 counts visible towers
vis([H|T], C) :- vis(T, H, 1, C).
vis([], _, C, C).
vis([H|T], Max, Temp, C) :-
(   H > Max
->  Temp2 #= Temp+1, vis(T, H, Temp2, C)
;   vis(T, Max, Temp, C)
).

% plain_tower/3 is like tower/3 but does not use the finite domain solver

% fill/2 creates the list [N,...,2,1]
fill(0,[]).
fill(N, [N|T]) :-
N>0, N2 is N - 1,
fill(N2, T).

plain_tower(N,T,C) :-
fill(N, Domain),
length2(N,T),
maplist(length2(N), T),
maplist(permutation(Domain), T),
transpose(T, Cols),
maplist(fd_all_different, T),
maplist(fd_all_different, Cols),
maplist(reverse, Cols, Col2),
maplist(reverse, T, T2),
maplist(vis, Cols,Top),
maplist(vis, Col2,Bot),
maplist(vis, T, Left),
maplist(vis, T2, Right),
C=counts(Top,Bot,Left,Right).

% speedup/1 compares the runtime for tower/3 and plain_tower/3

test1(T1) :-
statistics(runtime,[Start|_]),
tower(4,_,counts([1,2,2,4],[4,2,2,1],[1,2,2,4],[4,2,2,1])),
statistics(runtime,[Stop|_]),
T1 is Stop - Start.

test2(T2) :-
statistics(runtime,[Start|_]),
plain_tower(4,_,counts([1,2,2,4],[4,2,2,1],[1,2,2,4],[4,2,2,1])),
statistics(runtime,[Stop|_]),
T2 is Stop - Start.

speedup(R) :-
test1(T1),
test2(T2),
R is T2/T1.

% ambiguous/4 finds different tower puzzles with the same counts

ambiguous(N, C, T1, T2) :-
tower(N, T1, C),
tower(N, T2, C),
T1 \= T2.









