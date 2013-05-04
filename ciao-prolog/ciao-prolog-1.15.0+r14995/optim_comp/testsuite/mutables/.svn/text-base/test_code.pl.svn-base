
main :-
	'$ctx_with'(empty_mutstore, (test, test2)).

:- '$context'(test/0, mutstore).
test :-
	X = ~newmut(integer, 10),
	Y = ~newmut(integer, 10),
	( X == Y -> % TODO: it should be X = Y, but mut__* not allow this yet! (there is a bug)
	    display('wrong: X and Y are not the same mutvar'), nl
	; display('ok: X and Y are different mutvars'), nl
	),
	X <- 20,
	X1 is @X + 1,
	X <- X1,
	catch(X <- a, E, (display('ok: X cannot be assigned with an atom, exception caught: '), display(E), nl)),
	myread(X, Xv), display(should_be_the_same(Xv, 21)), nl.

:- '$context'(myread/2, mutstore_ro).
myread(X, V) :-
	V = @X.

:- '$context'(test2/0, mutstore).
test2 :-
	% creates the infinite list with elements [1,2,1,2,...]
	Z = ~newmut(term, nil),
	Z <- cons(1, W),
	W = ~newmut(term, nil),
	W <- cons(2, Z),
	% takes and prints the first 3 elements
	take_n(Z, 3, Xs),
	display(should_be_the_same([1, 2, 1], Xs)), nl,
	% modifies the node W so that the list Z ends with just one element
	W <- nil,
	% takes and prints the first 3 elements
	take_n(Z, 3, Xs2),
	display(should_be_the_same([1], Xs2)), nl.

:- '$context'(take_n/3, mutstore_ro).
% take the N first elements (if possible)
take_n(_, 0, []) :- !.
take_n(L, N, Xs) :-
	@L = Xv,
	( Xv = cons(X, L0) ->
	    Xs = [X|Xs0],
	    N1 is N - 1,
	    take_n(L0, N1, Xs0)
	; Xv = nil ->
	    Xs = []
	).


