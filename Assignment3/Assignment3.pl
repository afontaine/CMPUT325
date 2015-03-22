% setDifference takes two sets and returns a set of the elements in S1 that are
% not in S2. 
setDifference([], _, []).
setDifference([A|S1], S2, S3) :-
	member(A, S2),
	setDifference(S1, S2, S3).
setDifference([A|S1], S2, [A|S3]) :-
	setDifference(S1, S2, S3).

% Swap takes in a list and returns a list where each element is swapped with the
% element next to it.
swap([], []).
swap([A], [A]).
swap([A, B|L], [B, A|R]) :-
	swap(L, R).

% rmDup takes in a list and returns the list with all duplicates removed.
rmDup([], []).
rmDup([A|L], R) :-
	member(A, L),
	rmDup(L, R).
rmDup([A|L], [A|R]) :-
	rmDup(L, R).

% rmAllDup removes duplicates from a list with nested lists.
rmAllDup([], [], S).
rmAllDup([A|L], [X|R], S) :-
	\+ atomic(A),
	append([], S, T),
	rmAllDup(A, X, T),
	flatten(X, Y),
	append(S, Y, Z),
	rmAllDup(L, R, Z).
rmAllDup([A|L], [A|R], S) :-
	\+ member(A, S),
	append(S, [A], T),
	rmAllDup(L, R, T).
rmAllDup([_|L], R, S) :-
	rmAllDup(L, R, S).
rmAllDup(L, R) :-
	rmAllDup(L, R, []).

flatten([], []).
flatten([A|L], [A|R]) :-
	atomic(A), flatten(L, R).
flatten([A|L], R) :-
	flatten(A, A1), flatten(L, L1), append(A1, L1, R).
