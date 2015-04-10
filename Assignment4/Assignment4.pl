% Data for testing purposes.
c325(fall_2014,aperf,15,15,15,15,79,99).
c325(fall_2014,john,14,13,15,10,76,87).
c325(fall_2014,lily, 9,12,14,14,76,92).
c325(fall_2014,peter,8,13,12,9,56,58).
c325(fall_2014,ann,14,15,15,14,76,95).
c325(fall_2014,ken,11,12,13,14,54,87).
c325(fall_2014,kris,13,10,9,7,60,80).
c325(fall_2014,audrey,10,13,15,11,70,80).
c325(fall_2014,randy,14,13,11,9,67,76).
c325(fall_2014,david,15,15,11,12,66,76).
c325(fall_2014,sam,10,13,10,15,65,67).
c325(fall_2014,kim,14,13,12,11,68,78).
c325(fall_2014,perf,15,15,15,15,80,100).
c325(winter_2014,aperf,15,15,15,15,80,99).
setup(fall_2014,as1,15,0.1).
setup(fall_2014,as2,15,0.1).
setup(fall_2014,as3,15,0.1).
setup(fall_2014,as4,15,0.1).
setup(fall_2014,midterm,80,0.25).
setup(fall_2014,final,100,0.35).

query1(S, N, T) :-
	setup(S, as1, M1, W1),
	setup(S, as2, M2, W2),
	setup(S, as3, M3, W3),
	setup(S, as4, M4, W4),
	setup(S, midterm, MM, WM),
	setup(S, final, MF, WF),
	c325(S, N, R1, R2, R3, R4, RM, RF),
	T is R1 / M1 * W1 + R2 / M2 * W2 + R3 / M3 * W3 + R4 / M4 * W4 + RM / MM * WM + RF / MF * WF.
