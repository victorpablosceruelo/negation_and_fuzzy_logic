:- module(dec,
	[
	    analysis_check/3,
	    insert_symbol_dec/3
	], [assertions]).

%
%  dec.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for handling the declarations.
%

% Some reorganization by Edison Mera, June 2006.

:- use_module(infercost(init(dec_basic)), [legal_measure_symbol/3]).
:- use_module(infercost(top(utility)), [list/1, nonlist/1]).
:- use_module(infercost(top(error)), [error_message/3]).
:- use_module(infercost(init(symtable)), 
	[
	    insert_symbol_field/4,
	    find_symbol_field/4
	]).

%
%  Insert a declaration into the symbol table.
%
insert_symbol_dec(ST, Clause:_Key, Error) :-
	arg(1, Clause, Dec),
	functor(Dec, F, A),
	arg(1, Dec, Pred),
	(
	    A > 1 ->
	    arg(2,Dec,DecList)
	;
	    DecList = []
	),
	legal_declaration(F/A, Pred, DecList, ST, Clause, Error).

%
%  Test if a declaration is legal.
%
legal_declaration((mode)/2,F/N,Mode,ST,Clause,Error) :-
	!,
	(legal_mode_dec(Mode,N,Clause) ->
		(insert_symbol_field(ST,F/N,(mode),Mode),
		 Error = 1);
		Error = 0).
legal_declaration(measure/2,F/N,Measure,ST,Clause,Error) :-
	!,
	(legal_measure_dec(Measure,N,Clause) ->
		(insert_symbol_field(ST,F/N,measure,Measure),
		 Error = 1);
		Error = 0).
/*
legal_declaration(mutex/2,F/N,Mutex,ST,Clause,Error) :-
	!,
	(legal_mutex_dec(Mutex,N,Clause) ->
		(insert_symbol_field(ST,F/N,mutex,Mutex),
		 Error = 1);
		Error = 0).

legal_declaration(size/2,F/N,Size,ST,Clause,Error) :-
	!,
	(legal_size_dec(Size,N,Clause) ->
		(in_comp_dec(Size,NSize,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,size,NSize);
			error_message(dec8,Size,Clause)));
		Error = 0).
*/
legal_declaration(det/1,F/N,_,ST,_,Error) :-
	!,
	Error = 1,
	insert_symbol_field(ST,F/N,det,[1]).

/*
	(legal_det_dec(Det,N,Clause) ->
		(in_comp_dec(Det,NDet,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,det,[NDet]);
			error_message(dec8,Det,Clause)));
		Error = 0).
*/

/*
legal_declaration(time/2,F/N,Time,ST,Clause,Error) :-
	(legal_time_dec(Time,N,Clause) ->
		(in_comp_dec(Time,NTime,Error),
		 (Error =:= 1 ->
			insert_symbol_field(ST,F/N,time,[NTime]);
			error_message(dec8,Time,Clause)));
		Error = 0).
*/
legal_declaration((domain)/2,F/N,Domain,ST,Clause,Error) :-
	!,
	(legal_domain_dec(Domain,N,Clause) ->
		(insert_symbol_field(ST,F/N,domain,Domain),
		 Error = 1);
		Error = 0).
 %% legal_declaration(Dec,_,_,_,Clause,0) :-
 %% 	Dec \== (mode)/2,
 %% 	Dec \== measure/2,
 %% %	Dec \== mutex/2,
 %% %	Dec \== size/2,
 %% 	Dec \== det/1,
 %% %	Dec \== time/2,
 %% 	Dec \== domain/2,
 %% 	error_message(dec2,Dec,Clause).
legal_declaration(_Dec,_,_,_,_Clause,1).

%
%  Detect the mode declaration error.
%
legal_mode_dec(Mode,N,Clause) :-
	utility:list(Mode),
	!,
	legal_mode_symbol(Mode,N,Clause).
legal_mode_dec(Mode,_,Clause) :-
	nonlist(Mode),
	error_message(mode1,_,Clause),
	fail.

legal_mode_symbol([],0,_).
legal_mode_symbol([],N,Clause) :-
	N =\= 0,
	!,
	error_message(mode2,_,Clause),
	fail.
legal_mode_symbol([_|_],0,Clause) :-
	!,
	error_message(mode2,_,Clause),
	fail.
legal_mode_symbol([(+)|Mode],N,Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_mode_symbol(Mode,N1,Clause).
legal_mode_symbol([(-)|Mode],N,Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_mode_symbol(Mode,N1,Clause).
legal_mode_symbol([M|_],_,Clause) :-
	M \== '+',
	M \== '-',
	error_message(mode3,M,Clause),
	fail.

%
%  Detect the measure declaration error.
%
legal_measure_dec(Measure,N,Clause) :-
	utility:list(Measure),
	!,
	legal_measure_symbol(Measure,N,Clause).
legal_measure_dec(Measure,_,Clause) :-
	nonlist(Measure),
	error_message(measure1,_,Clause),
	fail.

/*
%
%  Detect the mutex declaration error.
%
legal_mutex_dec(Mutex,_,Clause) :-
	utility:list(Mutex),
	!,
	legal_mutex_dec1(Mutex,_,Clause).
legal_mutex_dec(Mutex,_,Clause) :-
	nonlist(Mutex),
	error_message(mutex1,_,Clause),
	fail.

legal_mutex_dec1([],_,_).
legal_mutex_dec1([M|Mutex],_,Clause) :-
	(utility:list(M) ->
		(legal_mutex_dec2(M,_,Clause),
		 legal_mutex_dec1(Mutex,_,Clause));
		(error_message(mutex1,_,Clause),
		 fail)).

legal_mutex_dec2([],_,_).
legal_mutex_dec2([M|Ms],_,Clause) :-
	(integer(M) ->
		legal_mutex_dec2(Ms,_,Clause);
		(error_message(mutex2,_,Clause),
		 fail)).
*/
%
legal_domain_dec(Domain,N,Clause) :-
	utility:list(Domain),
	!,
	legal_domain_symbol(Domain,N,Clause).

/*
legal_size_dec(Domain,_,Clause) :-
	nonlist(Domain),
	error_message(domain1,_,Clause),
	fail.
*/
legal_domain_symbol([],0,_).
legal_domain_symbol([],N,Clause) :-
	N =\= 0,
	!,
	error_message(domain2,_,Clause),
	fail.
legal_domain_symbol([_|_],0,Clause) :-
	!,
	error_message(domain2,_,Clause),
	fail.
legal_domain_symbol([D|Domain],N,Clause) :-
	N > 0,
	!,
	(legal_domain_symbol1(D,N,Clause) ->
		(N1 is N-1,
		 legal_domain_symbol(Domain,N1,Clause));
		(error_message(domain3,_,Clause),
		 fail)).

legal_domain_symbol1((L-U),_,_) :-
	integer(L),
	integer(U),
	L =< U,
	!.
legal_domain_symbol1(D,_,_) :-
	utility:list(D).

%
%  Detect the size declaration error.
%

:- discontiguous legal_size_dec/3.
/*
legal_size_dec(Size,N,Clause) :-
	utility:list(Size),
	!,
	legal_size_symbol(Size,N,Clause).
legal_size_dec(Size,_,Clause) :-
	nonlist(Size),
	error_message(size1,_,Clause),
	fail.

legal_size_symbol([],0,_).
legal_size_symbol([],N,Clause) :-
	N =\= 0,
	!,
	error_message(size2,_,Clause),
	fail.
legal_size_symbol([_|_],0,Clause) :-
	!,
	error_message(size2,_,Clause),
	fail.
legal_size_symbol([_|Size],N,Clause) :-
	N > 0,
	N1 is N-1,
	legal_size_symbol(Size,N1,Clause).

%
%  Detect the det declaration error.
%
legal_det_dec(Det,_,Clause) :-
	Det \== 1,
	!,
	error_message(det1,_,Clause),
	fail.
legal_det_dec(1,_,_).

%
%  Detect the time declaration error.
%
legal_time_dec(Time,_,Clause) :-
	utility:list(Time),
	!,
	error_message(time1,_,Clause),
	fail.
legal_time_dec(Time,_,_) :-
	nonlist(Time).

%
%  Format the input complexity declaration.
%
input_comp_dec([],[],1).
input_comp_dec([Exp|EList],[NExp|NList],Error) :-
	in_comp_dec(Exp,NExp,Error1),
	input_comp_dec(EList,NList,Error2),
	Error is Error1*Error2.



in_comp_dec(Exp,Exp,0) :-
	var(Exp),
	!.
in_comp_dec(Exp,Exp,1) :-
	atomic(Exp),
	!.
in_comp_dec(Exp,NExp,Error) :-
	compound(Exp),
	!,
	functor(Exp,F,N),
	(F/N == '$'/1 ->
		(arg(1,Exp,Arg),
		 (integer(Arg) ->
			NExp =.. ['$',0,Arg];
			NExp = Exp),
		 Error = 1);
		(functor(NExp,F,N),
		 in_comp_dec(N,Exp,NExp,Error))).

:- push_prolog_flag(multi_arity_warnings,off).

in_comp_dec(0,_,_,1).
in_comp_dec(N,Exp,NExp,Error) :-
	N > 0,
	arg(N,Exp,Arg),
	in_comp_dec(Arg,NArg,Error1),
	arg(N,NExp,NArg),
	N1 is N-1,
	in_comp_dec(N1,Exp,NExp,Error2),
	Error is Error1*Error2.

:- pop_prolog_flag(multi_arity_warnings).
*/
%
%  Check if the mode declarations for the predicates of a SCC are declared.
%
mode_declared([],_,1).
mode_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,(mode),Mode),
	(var(Mode) -> 
		(error_message(dec3,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	mode_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the measure declarations for the predicates of a SCC are declared.
%
measure_declared([],_,1).
measure_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,measure,Measure),
	(var(Measure) -> 
		(
                 % Added by PLG, Oct-28-04
                 error_message(dec4,Pred,''), % JNL
					      % Commented out by PLG, Oct-28-04
                                              % Don't raise any message.
                 Pred = _F/A,
                 default_measure(A, Measure),
                 Error1 = 1);
                 % End added.
		 % Error1 = 0); % Commented out by PLG, Oct-28-04
		Error1 = 1),
	measure_declared(Comp,ST,Error2),
	Error is Error1*Error2.

% Added by PLG, Oct-28-04
default_measure(0, []).
% default_measure(N, [void|R]):-
default_measure(N, [size|R]):-
      N > 0,
      N1 is N - 1,
      default_measure(N1, R).
% End added.

/*
%
%  Check if the size declarations for the predicates of a SCC are declared.
%
size_declared([],_,1).
size_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,size,Size),
	(var(Size) -> 
		(error_message(dec5,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	size_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the det declarations for the predicates of a SCC are declared.
%
det_declared([],_,1).
det_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,det,Det),
	(var(Det) -> 
		(error_message(dec6,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	det_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the time declarations for the predicates of a SCC are declared.
%
time_declared([],_,1).
time_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,time,Time),
	(var(Time) -> 
		(error_message(dec7,Pred,''),
		 Error1 = 0);
		Error1 = 1),
	time_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
%  Check if the mutex declarations for the predicates of a SCC are declared.
%
mutex_declared([],_,1).
mutex_declared([Pred|Comp],ST,Error) :-
	find_symbol_field(ST,Pred,mutex,Mutex),
	(var(Mutex) -> 
		(find_symbol_field_clause(ST,Pred,Clauses),
		 default_mutex(Clauses,1,DMutex),
		 insert_symbol_field(ST,Pred,mutex,DMutex),
		 Error1 = 1);
		Error1 = 1),
	mutex_declared(Comp,ST,Error2),
	Error is Error1*Error2.

%
default_mutex(Clauses,_,[]) :-
	var(Clauses),
	!.
default_mutex(Clauses,N,[[N]|DMutex]) :-
	nonvar(Clauses),
	Clauses = [_|Cs],
	N1 is N+1,
	default_mutex(Cs,N1,DMutex).
*/
%
%  Check if the program is well-declared.
%
analysis_check([],_,1).
analysis_check([Comp|CompList],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	analysis_check(CompList,ST,Error3),
	Error is Error1*Error2*Error3.

/*
%
%  Check if the program is well-declared for size analysis.
%
size_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	!,
	Error is Error1*Error2.
size_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	size_analysis_check(CompList,ST,Error4),
	Error is Error1*Error2*Error3*Error4.

%
%  Check if the program is well-declared for solution analysis.
%
solution_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	mutex_declared(Comp,ST,Error4),
	!,
	Error is Error1*Error2*Error3*Error4.
solution_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	mutex_declared(Comp,ST,Error5),
	solution_analysis_check(CompList,ST,Error6),
	Error is Error1*Error2*Error3*Error4*Error5*Error6.

%
%  Check if the program is well-declared for time analysis.
%
time_analysis_check([Comp],ST,Error) :-
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	mutex_declared(Comp,ST,Error5),
	!,
	Error is Error1 * Error2 * Error3 * Error4 * Error5.
time_analysis_check([Comp|CompList],ST,Error) :-
	CompList \== [],
	mode_declared(Comp,ST,Error1),
	measure_declared(Comp,ST,Error2),
	size_declared(Comp,ST,Error3),
	det_declared(Comp,ST,Error4),
	time_declared(Comp,ST,Error5),
	mutex_declared(Comp,ST,Error6),
	time_analysis_check(CompList,ST,Error7),
	Error is Error1 * Error2 * Error3 * Error4 * Error5 * Error6 * Error7.
*/
