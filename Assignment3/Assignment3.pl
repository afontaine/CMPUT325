% setDifference takes two sets and returns a set of the elements in S1 that are
% not in S2. 
% Base case: If both lists is empty, return empty list.
setDifference([], [], []).
% If the atom we are at exists in the second list, ignore it.
setDifference([A|S1], S2, S3) :-
	member(A, S2),
	!,
	setDifference(S1, S2, S3).
% If the above fails, we are at an element not present in the second list.
% Append it to our result.
setDifference([A|S1], S2, [A|S3]) :-
	setDifference(S1, S2, S3).

% Swap takes in a list and returns a list where each element is swapped with the
% element next to it.
% Base case: If list is empty return empty list.
swap([], []).
% Base case: If list has single element, return single element list.
swap([A], [A]) :- !.
% List has at least 2 elements. Take first 2 and swap them and continue.
swap([A, B|L], [B, A|R]) :-
	swap(L, R).

% rmDup takes in a list and returns the list with all duplicates removed.
% Base case: If empty list, return empty list.
rmDup([], []).
% If the atom we are at exists later in the list, we ignore it, as we pull
% the last available instance of that atom.
rmDup([A|L], R) :-
	member(A, L),
	!,
	rmDup(L, R).
% The above rules have failed and we reach an atom which does not exist later
% in the list. The atom is now unique and can be appended to our result list.
rmDup([A|L], [A|R]) :-
	rmDup(L, R).

% rmAllDup removes duplicates from a list with nested lists.
% Base case: If empty list, return empty list.
rmAllDup([], [], _).
% S contains all atoms seen so far. If A is a list,
% we remove all the duplicates in it, and append the result
% to our ongoing result AND a flattened version to our "seen
% so far" list.
rmAllDup([A|L], [X|R], S) :-
	\+ atomic(A),
	!,
	rmAllDup(A, X, S),
	flatten(X, Y),
	append(S, Y, V),
	rmAllDup(L, R, V).
% If we reach a new atom, we add it to our result list and the "seen so far"
% list.
rmAllDup([A|L], [A|R], S) :-
	\+ member(A, S), !,
	rmAllDup(L, R, [A|S]).
% If all above fails, we've reached an atom we've seen before. Continue,
% ignoring the atom.
rmAllDup([_|L], R, S) :-
	rmAllDup(L, R, S).
% Start the process with an empty "seen so far" list.
rmAllDup(L, R) :-
	rmAllDup(L, R, []).

% Takes in a list and returns it flattened to one level.
% Pulled from class notes.
flatten([], []).
flatten([A|L], [A|R]) :-
	atomic(A), !, flatten(L, R).
flatten([A|L], R) :-
	flatten(A, A1), flatten(L, L1), append(A1, L1, R).

% Takes in a (potentially nested) list of numbers and 'smallest' or 'largest',
% and returns either the smallest or largest number in the list.

% Large finds the largest element in a nested list.
% If we've reached the end, return our largest number.
large([], A, N) :-
	N = A.
% If first element is an atom and bigger than our biggest found number,
% it becomes the new biggest number
large([F|L], A, N) :-
	atomic(F),
	F >= A,
	!,
	large(L, F, N).
% If first element is an atom and smaller than our biggest found number,
% ignore and move on.
large([F|L], A, N) :-
	atomic(F),
	!,
	large(L, A, N).
% If first element is a nested list, we find the largest element of it. If the
% largest element is larger than our biggest so far found, it becomes the new
% biggest number.
large([F|L], A, N) :-
	large(F, A, M),
	M >= A,
	!,
	large(L, M, N).
% If the first element is a nested list, we can assume the above rule failed,
% the largest element in it is smaller than what we have, and we can move on.
large([_|L], A, N) :-
	!,
	large(L, A, N).

% Small finds the smallest element in a nested list.
% If the list is empty, return our smallest found number.
small([], A, N) :-
	N = A.
% If the first element is an atom and smaller than our smallest number,
% it becomes the new smallest.
small([F|L], A, N) :-
	atomic(F),
	F =< A,
	!,
	small(L, F, N).
% If the first element is an atom, we can assume the above rule failed at that
% it is bigger than our so far smallest. We ignore it and continue.
small([F|L], A, N) :-
	atomic(F),
	!,
	small(L, A, N).
% If the first element is a nested list, we find the smallest element in it and
% if it is smaller than our smallest, it is the new smallest going forward.
small([F|L], A, N) :-
	small(F, A, M),
	M =< A,
	!,
	small(L, M, N).
% If the above rules failed, we can assume that we have a nested list whose
% smallest is bigger than the so-far-smallest. We ignore it and continue.
small([_|L], A, N) :-
	!,
	small(L, A, N).

% If our first element is an atom and they want the largest element in the list,
% we pass in the first element as the largest found and return the largest
% element.
generate([F|L], largest, N) :-
	atomic(F),
	!,
	large(L, F, N).
% Same as above, but they want the smallest element, so we find the smallest
% element.
generate([F|L], smallest, N) :-
	atomic(F),
	!,
	small(L, F, N).
% If the first element is a nested list and they want the largest element in a
% list, we find the largest element of the nested list and the rest of the list,
% compare them, and return the larger.
generate([[F|L2]|L], largest, N) :-
	large(L2, F, M),
	large(L, F, O),
	M >= O,
	!,
	N = M.
% Same as above, but smallest element.
generate([[F|L2]|L], smallest, N) :-
	small(L2, F, M),
	small(L, F, O),
	M =< O,
	!,
	N = M.
% If the above rules failed, we know we have a nested list as the first element
% and its largest element is smaller than the largest element in the rest of the
% list.
generate([[F|_]|L], largest, N) :-
	large(L, F, O),
	!,
	N = O.
% Same as above but smallest.
generate([[F|_]|L], smallest, N) :-
	small(L, F, O),
	!,
	N = O.

% countAll takes a (possibly nested) list and returns an ordered list of pairs,
% with the atoms of the list paired with their frequency in the original list in
% descending frequency.

% Occurance finds all occurances of a given atom in the list.
% If the list is emtpy, return a tuple in the form [atom, frequency].
occurance(A, [], N, F) :-
	N = [A, F].
% If we've found an occurance of atom A, increase the frequency count and
% continue
occurance(A, [B|L], N, F) :-
	A == B,
	!,
	F1 is F + 1,
	occurance(A, L, N, F1).
% If we've found an atom that isn't A, continue
occurance(A, [_|L], N, F) :-
	!,
	occurance(A, L, N, F).

% Count iterates over the list, creating a list of the format
% [[Atom, Frequency], ...]
% Base case: return an empty list if the list is empty. Also return a list
% of all seen atoms so far.
count([], [], S, S).
% We've found an atom that we have already counted. Continue.
count([A|L], N, S, V) :-
	member(A, S),
	!,
	count(L, N, S, V).
% We've found a new atom. Count occurances of it, and append it to our list.
% Also append it to the list of seen atoms.
count([A|L], [X|N], S, V) :-
	!,
	occurance(A, L, X, 1),
	count(L, N, [A|S], V).
% countAll takes a list of atoms and returns a list of tuples with atom and
% frequency, sorted in descending order by frequency. As preserving the original
% structure of the list is unneccessary, we flatten the list first to ease
% the iterating. We also use a merge sort algorithm to sort the list.
countAll(L, R) :-
	flatten(L, F),
	count(F, N, [], _),
	merge_sort(N, R).

% Merge sort and halve were found here:
% http://kti.mff.cuni.cz/~bartak/prolog/sorting.html#merge
% Modified to sort tuples in the form of [Atom, Frequency]
% in descending order by frequency.
halve(L, A, B) :- hv(L, [], A, B).
hv(L, L, [], L). % for lists of even length
hv(L, [_|L], [], L).
hv([H|T], Acc, [H|L], B) :- !, hv(T, [_|Acc], L, B).

merge_sort([], []).
merge_sort([X], [X]).
merge_sort(L, S) :-
	L = [_, _ | _],
	halve(L, L1, L2),
	!,
	merge_sort(L1, S1),
	merge_sort(L2, S2),
	merge(S1, S2, S).
merge([], L, L).
merge(L, [], L) :- !, L \= [].
merge([[A1, F1]|T1], [[A2, F2]|T2], [[A1, F1]|T]) :-
	F1 >= F2,
	!,
	merge(T1, [[A2, F2]|T2], T).
merge([[A1, F1]|T1], [[A2, F2]|T2], [[A2, F2]|T]) :-
	F1 =< F2,
	!,
	merge([[A1, F1]|T1], T2, T).

% Convert takes in a list and returns a list with the following rules:
% 1. Anything between matching q's is left as is.
% 2. q's are always left as-is.
% 3. Any e's outside of matching q's are removed.
% 4. Anything else outside of matching q's become c's.
convert([], [], _). % For empty lists
% If we've reached a quote and we're not in quote mode, and there is
% a quote to match with later in the list and turn quote mode on.
convert([q|L], [q|R], off) :-
	member(q, L),
	!,
	convert(L, R, on).
% If we've reached a quote and we're not in quote mode and there are no more
% quotes in the list, do not turn on quote mode and leave the quote as is.
convert([q|L], [q|R], off) :-
	!,
	convert(L, R, off).
% If we've reached a space and we aren't in quote mode, remove the space.
convert([e|L], R, off) :-
	!,
	convert(L, R, off).
% If we've reached an atom that is not a quote or a space, and we are not in
% quote mode, then make it a 'c'.
convert([_|L], [c|R], off) :-
	!,
	convert(L, R, off).
% If we've reached a quote and quote mode is on, turn quote mode off, leaving
% the quote as is.
convert([q|L], [q|R], on) :-
	!,
	convert(L, R, off).
% If we've reached an atom that isn't a quote and quote mode is on, leave the
% atom as is.
convert([A|L], [A|R], on) :-
	!,
	convert(L, R, on).

% Start the conversion process with quote mode off.
convert(L, R) :-
	convert(L, R, off).
