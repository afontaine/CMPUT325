% setDifference takes two sets and returns a set of the elements in S1 that are
% not in S2. 
setDifference([], _, []).
setDifference([A|S1], S2, S3) :-
	member(A, S2),
	!,
	setDifference(S1, S2, S3).
setDifference([A|S1], S2, [A|S3]) :-
	setDifference(S1, S2, S3).

% Swap takes in a list and returns a list where each element is swapped with the
% element next to it.
swap([], []).
swap([A], [A]) :- !.
swap([A, B|L], [B, A|R]) :-
	swap(L, R).

% rmDup takes in a list and returns the list with all duplicates removed.
rmDup([], []).
rmDup([A|L], R) :-
	member(A, L),
	!,
	rmDup(L, R).
rmDup([A|L], [A|R]) :-
	rmDup(L, R).

% rmAllDup removes duplicates from a list with nested lists.
rmAllDup([], [], _).
rmAllDup([A|L], [X|R], S) :-
	\+ atomic(A),
	!,
	append([], S, T),
	rmAllDup(A, X, T),
	flatten(X, Y),
	rmAllDup(L, R, [S|Y]).
rmAllDup([A|L], [A|R], S) :-
	\+ member(A, S), !,
	rmAllDup(L, R, [A|S]).
rmAllDup([_|L], R, S) :-
	rmAllDup(L, R, S).
rmAllDup(L, R) :-
	rmAllDup(L, R, []).

% Takes in a list and returns it flattened to one level.
flatten([], []).
flatten([A|L], [A|R]) :-
	atomic(A), !, flatten(L, R).
flatten([A|L], R) :-
	flatten(A, A1), flatten(L, L1), append(A1, L1, R).

% Takes in a (potentially nested) list of numbers and 'smallest' or 'largest',
% and returns either the smallest or largest number in the list.
large([], A, N) :-
	N = A.
large([F|L], A, N) :-
	atomic(F),
	F >= A,
	!,
	large(L, F, N).
large([F|L], A, N) :-
	atomic(F),
	!,
	large(L, A, N).
large([F|L], A, N) :-
	large(F, A, M),
	M >= A,
	!,
	large(L, M, N).
large([_|L], A, N) :-
	!,
	large(L, A, N).
small([], A, N) :-
	N = A.
small([F|L], A, N) :-
	atomic(F),
	F =< A,
	!,
	small(L, F, N).
small([F|L], A, N) :-
	atomic(F),
	!,
	small(L, A, N).
small([F|L], A, N) :-
	small(F, A, M),
	M =< A,
	!,
	small(L, M, N).
small([_|L], A, N) :-
	!,
	small(L, A, N).

generate([F|L], largest, N) :-
	atomic(F),
	!,
	large(L, F, N).
generate([F|L], smallest, N) :-
	atomic(F),
	!,
	small(L, F, N).
generate([F|L], largest, N) :-
	generate(F, largest, M),
	generate(L, largest, O),
	M >= O,
	!,
	N = M.
generate([F|L], smallest, N) :-
	generate(F, smallest, M),
	generate(L, smallest, O),
	M =< O,
	!,
	N = M.
generate([F|L], largest, N) :-
	generate(F, largest, M),
	generate(L, largest, O),
	!,
	N = O.
generate([F|L], smallest, N) :-
	generate(F, smallest, M),
	generate(L, smallest, O),
	!,
	N = O.

% countAll takes a (possibly nested) list and returns an ordered list of pairs,
% with the atoms of the list paired with their frequency in the original list in
% descending frequency.
occurance(A, [], N, F) :-
	N = [A, F], !.
occurance(A, [B|L], N, F) :-
	atomic(B),
	A == B,
	F1 is F + 1,
	!,
	occurance(A, L, N, F1).
occurance(A, [B|L], N, F) :-
	atomic(B),
	!,
	occurance(A, L, N, F).
occurance(A, [B|L], N, F) :-
	occurance(A, B, [C, D], 0),
	FM is F + D,
	!,
	occurance(A, L, N, FM).

count([], [], S, S) :- !.
count([A|L], N, S, V) :-
	atomic(A),
	member(A, S),
	!,
	count(L, N, S, V).
count([A|L], [X|N], S, V) :-
	atomic(A),
	!,
	occurance(A, L, X, 1),
	count(L, N, [A|S], V).
count([A|L], N, S, V) :-
	count(A, M, S, H),
	!,
	append(H, S, X),
	count(L, O, X, V),
	!,
	append(M, O, N).
