:- module(term_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for terms").

:- doc(module, "A test for term creation and manipulation, in
   different scopes (e.g., nested modules).").

:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	multiarity_test,
        submod.display_test.

multiarity_test :-
        display("Multi-arity predicates can be defined"), nl,
        p,
        p(1),
        p(1,2).

:- push_prolog_flag(multi_arity_warnings, off).
p :- display(nullary_p), nl.
p(X) :- display(unary_p(X)), nl.
p(X,Y) :- display(binary_p(X,Y)), nl.
:- push_prolog_flag(multi_arity_warnings, on).

% Example: A 'boolean' module with local values 't' and 'f'. Those
%          terms are unique and cannot be created unless the module
%          has access to the 'boolean' module. This is very similar
%          to sorts or types.
:- module bool {
    :- local t/0.
    :- local f/0.

    % Testing access methods ('t' and 'f')
    % (not necessary)
    get_t := t.
    get_f := f.
}.

% A nested module (defining some local terms)
:- module submod {
    % TODO: like ':- local' in XSB: 
    :- local local0/0.
    :- local local1/2.
      
    display_test :-
	% Some terms implicitly defined (i.e., user terms)
    	L1 = [user1, user2, user3, user4(a,b)],
        display(L1), nl,
        display(~maplist(get_owner, L1)), nl,
	% Some non-user terms
	L2 = [~term_test.bool.get_t,
	      bool.t,
	      term_test.bool.t,
	      local0, local1(a,b)],
        display(L2), nl,
        display(~maplist(get_owner, L2)), nl,
    	L3 = [1,_A,"string"],
        display(L3), nl,
        display(~maplist(get_type, L3)), nl.
}.

% TODO: this requires hiord, test it before
maplist(_P, []) := [] :- !.
maplist(P, [X|Xs]) := [~P(X)| ~maplist(P, Xs)].

:- partial(get_owner/2, get_owner/0). % TODO: 'partial' declaration should not be necessary
get_owner(X) := Y :-
	( '$kind_of'(X, var) -> Y = none
	; '$kind_of'(X, t_num) -> Y = none
	; '$kind_of'(X, t_string) -> Y = none
	; Owner = ~X.'$owner_module',
	  Y = ~Owner.'$to_str'
	).

% TODO: implement type/2
:- partial(get_type/2, get_type/0). % TODO: 'partial' declaration should not be necessary
get_type(X) := Y :-
	( '$kind_of'(X, var) -> Y = "var"
	; '$kind_of'(X, t_num) -> Y = "t_num"
	; '$kind_of'(X, t_string) -> Y = "t_string"
	; Y = "other"
	).
