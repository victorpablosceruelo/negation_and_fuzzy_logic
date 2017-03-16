:- module(dec_res, [insert_symbol_dec/3], [assertions]).

%
%  dec.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for handling the declarations.
%

% Some reorganization by Edison Mera, June 2006.
% Commented out unused predicates by Edison Mera, December 2008.

:- use_module(resources(init_res(dec_basic_res)), [legal_measure_symbol/3]).
:- use_module(resources(top_res(utility_res)),    [list/1, nonlist/1]).
:- use_module(resources(top_res(error_res)), [error_message/3]).
:- use_module(resources(init_res(symtable_res)),  [insert_symbol_field/4]).

%
%  Insert a declaration into the symbol table.
%
insert_symbol_dec(ST, Clause:_Key, Error) :-
	arg(1, Clause, Dec),
	functor(Dec, F, A),
	arg(1, Dec, Pred),
	(
	    A > 1 ->
	    arg(2, Dec, DecList)
	;
	    DecList = []
	),
	legal_declaration(F/A, Pred, DecList, ST, Clause, Error).

%
%  Test if a declaration is legal.
%
legal_declaration(mode/2, F/N, Mode, ST, Clause, Error) :-
	!,
	( legal_mode_dec(Mode, N, Clause) ->
	    ( insert_symbol_field(ST, F/N, (mode), Mode),
		Error = 1 ) ;
	    Error = 0 ).
legal_declaration(measure/2, F/N, Measure, ST, Clause, Error) :-
	!,
	( legal_measure_dec(Measure, N, Clause) ->
	    ( insert_symbol_field(ST, F/N, measure, Measure),
		Error = 1 ) ;
	    Error = 0 ).
legal_declaration(det/1, F/N, _, ST, _, Error) :-
	!,
	Error = 1,
	insert_symbol_field(ST, F/N, det, [1]).
legal_declaration(domain/2, F/N, Domain, ST, Clause, Error) :-
	!,
	( legal_domain_dec(Domain, N, Clause) ->
	    ( insert_symbol_field(ST, F/N, domain, Domain),
		Error = 1 ) ;
	    Error = 0 ).
%% legal_declaration(Dec,_,_,_,Clause,0) :-
%% 	Dec \== (mode)/2,
%% 	Dec \== measure/2,
%% %	Dec \== mutex/2,
%% %	Dec \== size/2,
%% 	Dec \== det/1,
%% %	Dec \== time/2,
%% 	Dec \== domain/2,
%% 	error_message(dec2,Dec,Clause).
legal_declaration(_Dec, _, _, _, _Clause, 1).

%
%  Detect the mode declaration error.
%
legal_mode_dec(Mode, N, Clause) :-
	utility_res:list(Mode),
	!,
	legal_mode_symbol(Mode, N, Clause).
legal_mode_dec(Mode, _, Clause) :-
	nonlist(Mode),
	error_message(mode1, _, Clause),
	fail.

legal_mode_symbol([], 0, _).
legal_mode_symbol([], N, Clause) :-
	N =\= 0,
	!,
	error_message(mode2, _, Clause),
	fail.
legal_mode_symbol([_|_], 0, Clause) :-
	!,
	error_message(mode2, _, Clause),
	fail.
legal_mode_symbol([(+)|Mode], N, Clause) :-
	N > 0,
	!,
	N1 is N -1,
	legal_mode_symbol(Mode, N1, Clause).
legal_mode_symbol([(-)|Mode], N, Clause) :-
	N > 0,
	!,
	N1 is N -1,
	legal_mode_symbol(Mode, N1, Clause).
legal_mode_symbol([M|_], _, Clause) :-
	M \== '+',
	M \== '-',
	error_message(mode3, M, Clause),
	fail.

%
%  Detect the measure declaration error.
%
legal_measure_dec(Measure, N, Clause) :-
	utility_res:list(Measure),
	!,
	legal_measure_symbol(Measure, N, Clause).
legal_measure_dec(Measure, _, Clause) :-
	nonlist(Measure),
	error_message(measure1, _, Clause),
	fail.
legal_domain_dec(Domain, N, Clause) :-
	utility_res:list(Domain),
	!,
	legal_domain_symbol(Domain, N, Clause).
legal_domain_symbol([], 0, _).
legal_domain_symbol([], N, Clause) :-
	N =\= 0,
	!,
	error_message(domain2, _, Clause),
	fail.
legal_domain_symbol([_|_], 0, Clause) :-
	!,
	error_message(domain2, _, Clause),
	fail.
legal_domain_symbol([D|Domain], N, Clause) :-
	N > 0,
	!,
	( legal_domain_symbol1(D, N, Clause) ->
	    ( N1 is N -1,
		legal_domain_symbol(Domain, N1, Clause) ) ;
	    ( error_message(domain3, _, Clause),
		fail ) ).

legal_domain_symbol1((L-U), _, _) :-
	integer(L),
	integer(U),
	L =< U,
	!.
legal_domain_symbol1(D, _, _) :-
	utility_res:list(D).
