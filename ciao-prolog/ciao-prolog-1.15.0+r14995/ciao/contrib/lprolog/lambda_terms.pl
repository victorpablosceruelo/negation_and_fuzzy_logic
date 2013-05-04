:- module(lambda_terms,
	[
	    deref/3,
	    add_dummies/4,
	    make_lambda/4,
	    bind/3,
	    freshvar/2,
	    new_ref/2,
	    get_ref/3,
		%assign/2,
	    display_ref/2,
	    display_ref/1
	],
	[]).

:- use_module(library(write)).
:- use_module(engine(attributes)).

% Dereference a variable
deref(A, X, Env) :-
	% deref is only ever looking for ptr
	% don't need to thread env through it
	% so jut inline gthe get_attribute call
	% that get_ref makes
	get_ref(A, ptr(Y), Env),
	%get_attribute(A, ptr(Y)),
	!,
	deref(Y, X, Env).
deref(X, X, _Env).

% 
add_dummies(E, 0, _, E) :-
	!.
add_dummies(E, N, NL, [dum(X)|T]) :-
	N1 is N - 1,
	X is NL + N1,
	add_dummies(E, N1, NL, T).

% Construct an abstraction from the number of abstractions N and the body T
make_lambda(0, Term, Term, _Env):-
	!.
make_lambda(N, Term, Term2, Env) :-
	get_ref(Term, lam(N2, T), Env),
	!,
	N1 is N + N2,
	new_ref(lam(N1, T), Term2).
make_lambda(N, Term, Term2, _Env) :-
	new_ref(lam(N, Term), Term2).

% Binding a variable to a term
bind(V, T, Env) :-
	get_var(V, VE, Env),
	(
	    (VE == T) -> true
	;
	    assign(VE, ptr(T))
	).

% TODO: is there a better/more efficient way to do this?
get_var(V, V1, Env) :-
	get_attribute(V, tvar(Key)), !,
	get_env(Key, V1, Env).

get_var(V, V, _Env).

% Generating a fresh variable with a given time stamp
freshvar(X, Term) :-
	new_ref(var(_,X),Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a new ref by assigning an attribute to the variable
new_ref(Y, X) :-
	attach_attribute(X, Y).

% Returns the ref (attribute) of the variable
get_ref(X, Y, Env) :-
	get_attribute(X, tvar(Key)), !,
	get_env(Key, Ref, Env),
	get_ref(Ref, Y, Env).
get_ref(X, Y, _Env) :-
	get_attribute(X, Y).

get_env(Name, HORef, [ho_ref(Name, HORef) | _Env]) :- !.
get_env(Name, HORef, [ho_ref(_Name, _HORef) | Rest]) :- get_env(Name, HORef, Rest).


% Assigns a new value to the variable (detach the previous attribute and
% attach a new one
assign(X, Y) :-
	detach_attribute(X),
	attach_attribute(Var, Y),
	X = Var.

% Displays a ref
display_ref(X) :-
	display_ref(X, []).
display_ref(X, Env) :-
	flat_ref(X, V, Env),
	!,
	display(V),display('.').
flat_ref(X, V, Env) :-
	deref(X, Y, Env),
	get_ref(Y, R, Env),
	flat_rawterm(R, V, Env).
flat_ref([], [], _Env).
flat_ref([H1|T1], [H2|T2], Env) :-
	!,
	flat_ref(H1, H2, Env),
	flat_ref(T1, T2, Env).
% these two (bndg,dum) are actually just normal terms,
% not attributed variables
flat_ref(bndg(X,Y), bndg(V,Y), Env) :-
	!,
	flat_ref(X, V, Env).
flat_ref(dum(X), dum(X), _Env) :-
	!.
flat_rawterm(const(X,Y), const(X,Y), _Env) :-
	!.
flat_rawterm(var(X,Y), var(X,Y), _Env) :-
	!.
flat_rawterm(dB(X), dB(X), _Env) :-
	!.
flat_rawterm(lam(X,Y), lam(X,W), Env) :-
	!,
	flat_ref(Y, W, Env).
flat_rawterm(app(X,Y), app(V,W), Env) :-
	!,
	flat_ref(X, V, Env),
	flat_ref(Y, W, Env).
flat_rawterm(susp(X,Y,Z,T), susp(A,Y,Z,D), Env) :-
	!,
	flat_ref(X, A, Env),
	flat_ref(T, D, Env).
flat_rawterm(ptr(X), ptr(V), Env) :-
	!,
	flat_ref(X, V, Env).
flat_rawterm(X, X, _Env).


