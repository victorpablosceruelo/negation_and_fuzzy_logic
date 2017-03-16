%% Added by PL.

% bool_normalize(+F0, -F1) takes a Boolean formula F0 and transforms it to
% an equivalent formula F1 that is a disjunction of conjunctions.

bool_normalize(Fin, Fout) :-
	b_push_negation(Fin, F0),
	push_conj(F0, Fout).

% b_push_negation pushes negations inwards using De Morgan's rules.

b_push_negation(not(','(F1, F2)), ';'(G1, G2)) :-
	!,
	b_push_negation(not(F1), G1),
	b_push_negation(not(F2), G2).
b_push_negation(not(';'(F1, F2)), ','(G1, G2)) :-
	!,
	b_push_negation(not(F1), G1),
	b_push_negation(not(F2), G2).
b_push_negation(not(not(F)), G) :-
	!,
	b_push_negation(F, G).
b_push_negation(','(F1, F2), ','(G1, G2)) :-
	!,
	b_push_negation(F1, G1),
	b_push_negation(F2, G2).
b_push_negation(';'(F1, F2), ';'(G1, G2)) :-
	!,
	b_push_negation(F1, G1),
	b_push_negation(F2, G2).
b_push_negation(X, X).

push_neg_in_test(X, _Y) :-
	var(X),
	!,
	send_signal(wrong_neg_test(X)),
	fail.
% 	warning(['Test is an unexpected free variable']).
push_neg_in_test(\+(\+(X)), Y) :- !, push_neg_in_test(X, Y).
push_neg_in_test(\+(X),     Y) :- !, neg_test(X, Y).
push_neg_in_test(X,         Y) :- translate_test(X, Y).

%% neg_test(true, false). % These are not tests.
%% neg_test(fail, true).
%% neg_test(false, true).
neg_test(=(X,                   Y), '$noteq$'(X, Y)).
neg_test('term_compare:=='(X,   Y), '$noteq$'(X, Y)).
neg_test('term_compare:\\=='(X, Y), =(X,         Y)).
neg_test(is(X,                  Y), =\=(X,       Y)).
neg_test('arithmetic:=\\='(X,   Y), =:=(X,       Y)).
neg_test('arithmetic:=:='(X,    Y), =\=(X,       Y)).
neg_test('arithmetic:<'(X,      Y), >=(X,        Y)).
neg_test('arithmetic:>'(X,      Y), =<(X,        Y)).
neg_test('arithmetic:=<'(X,     Y), >(X,         Y)).
neg_test('arithmetic:>='(X,     Y), <(X,         Y)).
% For Sicstus
neg_test('term_typing:atom'(X),    '$notatom$'(X)).
neg_test('term_typing:integer'(X), '$notinteger$'(X)).
neg_test('term_typing:number'(X),  '$notnumber$'(X)).
% For CIAO
neg_test('basic_props:atm'(X), '$notatom$'(X)).
neg_test('basic_props:int'(X), '$notinteger$'(X)).
neg_test('basic_props:num'(X), '$notnumber$'(X)).
% Negated tests (common for Sicstus and CIAO).
neg_test('$noteq$'(X, Y),   =(X, Y)).
neg_test('$notinteger$'(X), integer(X)).
neg_test('$notnumber$'(X),  number(X)).
neg_test('$notatom$'(X),    atom(X)).

translate_test('term_compare:=='(X, Y),         =(X, Y)) :- !.
translate_test('term_compare:\\=='(X, Y),       '$noteq$'(X, Y)) :- !.
translate_test(=(X, Y),                         =(X, Y)) :- !.
translate_test(is(X, Y),                        =:=(X, Y)) :- !.
translate_test('arithmetic:=\\='(X, Y),         =\=(X, Y)) :- !.
translate_test('arithmetic:=:='(X, Y),          =:=(X, Y)) :- !.
translate_test('arithmetic:<'(X, Y),            <(X, Y)) :- !.
translate_test('arithmetic:>'(X, Y),            >(X, Y)) :- !.
translate_test('arithmetic:=<'(X, Y),           =<(X, Y)) :- !.
translate_test('arithmetic:>='(X, Y),           >=(X, Y)) :- !.
translate_test('arithmetic:arithexpression'(X), arithexpression(X)) :- !.
% For Sicstus
translate_test('term_typing:integer'(X), integer(X)) :- !.
translate_test('term_typing:number'(X),  number(X)) :- !.
translate_test('term_typing:atom'(X),    atom(X)) :- !.
% For CIAO
translate_test('basic_props:int'(X),    integer(X)) :- !.
translate_test('basic_props:num'(X),    number(X)) :- !.
translate_test('basic_props:atm'(X),    atom(X)) :- !.
translate_test('term_typing:atomic'(X), atomic(X)) :- !.
translate_test('basic_props:gnd'(X),    ground(X)) :- !.
% Negated tests (common for Sicstus and CIAO).
translate_test('$noteq$'(X, Y),   '$noteq$'(X, Y)) :- !.
translate_test('$notnumber$'(X),  '$notnumber$'(X)) :- !.
translate_test('$notinteger$'(X), '$notinteger$'(X)) :- !.
translate_test('$notatom$'(X),    '$notatom$'(X)) :- !.
translate_test(A,                 A) :- warning_message(
	    "No translation rule for ~w", [A]).

%% End of added by PL.

% 6 jul 99

remove_negation(\+(\+(X)), Y) :-
	!,
	remove_negation(X, Y).
remove_negation(\+('$'(Lit, _, _)), Lit) :-
	!.
remove_negation(\+(X), X) :-
	!.
remove_negation(X, X).

put_negation(\+(\+(X)), Literal, \+(\+(Test))) :-
	!,
	put_negation(X, Literal, Test).
put_negation(\+(_X), Literal, \+(Literal)) :-
	!.
put_negation(_X, Literal, Literal).
