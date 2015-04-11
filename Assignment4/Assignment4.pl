:- use_module(library(clpfd)).
:- use_module(library(lists)).
% Data for testing purposes.
room(r1).
room(r2).
room(r3).
notAtSameTime([b, i, h, g]).
before(i, j).
at(a, _, r2).

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
	MapLst = [a, b, c, d, e, f, g, h, i, j, k],
	findall(R1, room(R1), R),
	length(R, RoomLen),
	RoomNum is RoomLen + 9,
	length(MapLst, Len),
	length(TimeLst, Len),
	length(RmLst, Len),
	append(TimeLst, RmLst, W),
	findall(L, notAtSameTime(L), C1),
	findall([Q1, Q2], before(Q1, Q2), C2),
	findall([Session, Time, Rm], at(Session, Time, Rm), C3),
	TimeLst ins 1..4,
	RmLst ins 10..RoomNum,
	labeling([], TimeLst),
	labeling([], RmLst),
	constr1(TimeLst, C1, MapLst),
	constr2(TimeLst, C2, MapLst),
	constr3(TimeLst, RmLst, C3, MapLst),
	exclude(TimeLst, RmLst),
	labeling([], W).

constr1(_, [], _).
constr1(TimeLst, [C|C0], MapLst) :-
	mapTimes(C, TimeLst, MapLst, R),
	all_distinct(R),
	constr1(TimeLst, C0, MapLst).

mapTimes([], _, _, []).
mapTimes([D|Dom], TimeLst, MapLst, [X|R]) :-
	nth0(V, MapLst, D),
	nth0(V, TimeList, X),
	mapTimes(Dom, TimeLst, MapLst, R).

mapRooms([], []).
mapRooms([D|Dom], [X|R]) :-
	findall(R1, room(R1), R2),
	nth0(V, R2, D),
	X is V + 10,
	mapRooms(Dom, R).

mapSessToRoom([], _, _, []).
mapSessToRoom([S|Sess], RmLst, MapLst, [X|R]) :-
	nth0(V, MapLst, D),
	nth0(V, RmLst, X),
	mapSessToRoom(Sess, RmLst, MapLst, R).

constr2(_, [], _).
constr2(TimeLst, [[Q1, Q2] | C], MapLst) :-
	mapTimes([Q1, Q2], TimeLst, MapLst, [V, V2]),
	V #> V2,
	constr2(TimeLst, C, MapLst).

constr3(_, _, [], _).
constr3(TimeLst, RmLst, [[S, T, R]|C], MapLst) :-
	mapTimes([S], TimeLst, MapLst, [T1]),
	mapRooms([R], [R1]),
	mapSessToRoom([S], RmLst, MapLst, [R2]),
	T1 #= T,
	R2 #= R1,
	constr3(TimeLst, RmLst, C, MapLst).


member([T, R], [[T1, R1]|L]) :-
	T #= T1 #/\ R #= R1.
member([T, R], [_|L]) :-
	member([T, R], L).

exclude(TimeLst, RoomLst) :-
	map(TimeLst, RoomLst, M),
	noPairs(M).

noPairs([]).
noPairs([P | M]) :-
	\+ member(P, M),
	noPairs(M).

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

map([],_,[]).
map([H1|X], [H2|Y], [[H1,H2]|M]) :-
	map(X, Y, M).
