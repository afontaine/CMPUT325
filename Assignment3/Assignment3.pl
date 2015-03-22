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
