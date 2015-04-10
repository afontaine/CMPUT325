:- use_module(library(clpfd)).
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


