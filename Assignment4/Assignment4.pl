:- use_module(library(clpfd)).
:- use_module(library(lists)).
% Data for testing purposes.
insert_data :-
	assert(c325(fall_2014,aperf,15,15,15,15,79,99)),
    assert(c325(fall_2014,john,14,13,15,10,76,87)),
    assert(c325(fall_2014,lily, 9,12,14,14,76,92)),
    assert(c325(fall_2014,peter,8,13,12,9,56,58)),
    assert(c325(fall_2014,ann,14,15,15,14,76,95)),
    assert(c325(fall_2014,ken,11,12,13,14,54,87)),
    assert(c325(fall_2014,kris,13,10,9,7,60,80)),
    assert(c325(fall_2014,audrey,10,13,15,11,70,80)),
    assert(c325(fall_2014,randy,14,13,11,9,67,76)),
    assert(c325(fall_2014,david,15,15,11,12,66,76)),
    assert(c325(fall_2014,sam,10,13,10,15,65,67)),
    assert(c325(fall_2014,kim,14,13,12,11,68,78)),
    assert(c325(fall_2014,perf,15,15,15,15,80,100)),
    assert(c325(winter_2014,aperf,15,15,15,15,80,99)),
    assert(setup(fall_2014,as1,15,0.1)),
    assert(setup(fall_2014,as2,15,0.1)),
    assert(setup(fall_2014,as3,15,0.1)),
    assert(setup(fall_2014,as4,15,0.1)),
    assert(setup(fall_2014,midterm,80,0.25)),
    assert(setup(fall_2014,final,100,0.35)).

query1(S, N, T) :-
	setup(S, as1, M1, W1),
	setup(S, as2, M2, W2),
	setup(S, as3, M3, W3),
	setup(S, as4, M4, W4),
	setup(S, midterm, MM, WM),
	setup(S, final, MF, WF),
	c325(S, N, R1, R2, R3, R4, RM, RF),
	T is R1 / M1 * W1 + R2 / M2 * W2 + R3 / M3 * W3 + R4 / M4 * W4 + RM / MM * WM + RF / MF * WF.

query2(S, L) :-
	findall(N, query2name(S, N), L).

query2name(S, N) :-
	setup(S, midterm, MM, _),
	setup(S, final, MF, _),
	c325(S, N, _, _, _, _, M, F),
	M / MM < F / MF.

query3(S, N, as1, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, X, A2, A3, A4, M, F)).
query3(S, N, as2, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, A1, X, A3, A4, M, F)).
query3(S, N, as3, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, A1, A2, X, A4, M, F)).
query3(S, N, as4, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, A1, A2, A3, X, M, F)).
query3(S, N, midterm, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, A1, A2, A3, A4, X, F)).
query3(S, N, final, X) :-
	c325(S, N, A1, A2, A3, A4, M, F),
	retract(c325(S, N, A1, A2, A3, A4, M, F)),
	assert(c325(S, N, A1, A2, A3, A4, M, X)).
query3(_, _, _, _) :-
	print('record not found').

schedule(TimeLst, RmLst) :-
	TimeLst = [A, B, C, D, E, F, G, H, I, J, K],
	MapLst = [a, b, c, d, e, f, g, h, i, j, k],
	findall(R1, room(R1), R),
	length(R, RoomLen),
	RoomNum is RoomLen + 10,
	length(TimeLst, Len),
	length(RmLst, Len),
	append(TimeLst, RmLst, W),
	findall(L, notAtSameTime(L), C1),
	findall([Q1, Q2], before(Q1, Q2), C2),
	findall([Session, Time, Rm], at(Session, Time, Rm), C3),
	TimeLst ins 1..4,
	RmLst ins 10..RoomNum,
	constr1(TimeLst, C1, MapLst),
	constr2(TimeLst, C2, MapLst),
	constr3(TimeLst, RmLst, C3, MapLst),
	exclusive(TimeLst, RmLst),
	labeling([], W).

head([H|T], H).
constr1(_, [], _).
constr1(TimeLst, [[]|C], MapLst) :-
	constr1(TimeLst, C, MapLst).
constr1(TimeLst, [[C|C1]|C0], MapLst) :-
	nth0(X, MapLst, C),
	nth0(X, TimeLst, V),
	head(C1, H),
	nth0(Y, MapList, H),
	nth0(Y, TimeLst, V1),
	V #\= V1,
	constr1(TimeLst, [C1|C0], MapLst).

constr2(_, [], _).
constr2(TimeLst, [[Q1, Q2] | C], MapLst) :-
	nth0(X, MapLst, Q1),
	nth0(X, TimeLst, V),
	nth0(Y, MapLst, Q2),
	nth0(Y, MapLst, V1),
	V #> V2,
	constr2(TimeLst, C, MapLst).

constr3(_, _, [], _).
constr3(TimeLst, RmLst, [[S, T, R]|C], MapLst) :-
	nth0(X, MapList, S),
	nth0(X, TimeLst, T1),
	nth0(X, RmLst, R1),
	T1 #= T,
	R1 #= R,
	constr3(TimeLst, RmLst, C, MapLst).

subsetSum(L, R) :-
	length(L, Len),
	length(V, Len),
	V ins 0..1,
	labeling([], V),
	mapList(L, V, R),
	sum_list(R, X),
	length(R, T),
	T #\=0,
	X #= 0.

mapList(_, [], []).
mapList([H|L], [1|T], [H|R]) :-
	mapList(L, T, R).
mapList([_|L], [0|T], R) :-
	mapList(L, T, R).
