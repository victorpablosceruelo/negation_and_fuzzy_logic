% NOTE: Include this module in a context where show_any/1 is defined.
% TODO: This should be a module, it is not.

:- export(display/1).
display(X) :-
	( var(X) -> show_any(X)
	; X = [] -> show_any("[]")
	; X = [_|_] -> show_any("["), show_list(X), show_any("]")
	; show_any(X)
	).
show_list(Qs) :-
	( Qs = [] -> true
	; Qs = [X|Xs] ->
	    ( var(Xs) -> display(X), show_any("|"), display(Xs)
	    ; Xs = [] -> display(X)
	    ; Xs = [_|_] -> display(X), show_any(","), show_list(Xs)
	    ; display(X), show_any("|"), display(Xs)
	    )
	).
