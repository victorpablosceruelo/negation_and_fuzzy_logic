:- module(term_size, [general_term_size/8], [assertions]).

%
%  term_size.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for determining the size of a general
%  term.
%

:- use_module(infercost(size(ground_size)), [ground_term_size/3]).
:- use_module(infercost(size(term_diff)), [general_term_diff/7]).
:- use_module(infercost(top(utility)), 
	[
	    compound/1,
	    intersection/3,
	    addition/3,
	    nonlist/1,
	    member/2,
	    maximum/3
	]).
:- use_module(infercost(dependency(gvars)), [find_gvars_field/4]).
:- use_module(infercost(cost_approx)).

%
%  Determine the size of a general term.
%
general_term_size(void,   _Clause, _BT, _ST, _Gvars, _PosSet, _Term, 0) :-
	!.
general_term_size(Measure, Clause,  BT,  ST,  Gvars,  PosSet,  Term, Size) :-
	Measure \== void,
	ground_term_size(Measure, Term, Size1),
	(
	    Size1 == bot ->
	    nonground_term_size(Measure, Clause, BT, ST, Gvars, PosSet,
	        Term, Size)
	;
	    Size = Size1
	).

%
%  Determine the size of a general nonground term.
%

 %% Original version (upper bounds) commented out by PLG
 %% 
 %% nonground_term_size(Measure, Clause, BT, ST, Gvars, PosSet, Term, Size) :-
 %% 	common_predecessor(Term, Gvars, PosSet, ComPred),
 %% 	general_term_diff(BT, ST, Measure, Clause, ComPred, Term, Size1),
 %% 	(Size1 == bot, compound(Term) ->
 %% 		general_subterm_size(Measure, Clause, BT, ST, Gvars, PosSet,
 %% 				     Term, Size);
 %% 		Size = Size1).

% Added by PLG
nonground_term_size(Measure, Clause, BT, ST, Gvars, PosSet, Term, Size) :-
	common_predecessor(Term, Gvars, PosSet, ComPred),
	general_term_diff(BT, ST, Measure, Clause, ComPred, Term, Size1),
	(
	    Size1 == bot, compound(Term) ->
	    general_subterm_size(Measure, Clause, BT, ST, Gvars, PosSet,
	        Term, Size2)
	;
	    Size2 = Size1
	),
        set_zero_if_lower_bot(Size2, Size).

set_zero_if_lower_bot(X, Y):-
      approximation(Approx),
      (Approx == lower, X == bot -> 
           Y = 0
           % normal_form(0, Y)
           ;
           Y = X).

%End added.
%
%  Determine the common predecessors of the variables in a term.
%
common_predecessor(Term, _, ComPred, ComPred) :-
	atomic(Term),
	!.
common_predecessor(Term, Gvars, ComPred, NewComPred) :-
	var(Term),
	!,
	find_gvars_field(Gvars, Term, def, Pos),
	intersection(ComPred, Pos, NewComPred).
common_predecessor(Term, Gvars, ComPred, NewComPred) :-
	compound(Term),
	functor(Term, _, N),
	common_predecessor(N, Term, Gvars, ComPred, NewComPred).

:- push_prolog_flag(multi_arity_warnings, off).

common_predecessor(0, _, _, ComPred, ComPred) :-
	!.
common_predecessor(N, Term, Gvars, ComPred, NewComPred) :-
	N > 0,
	arg(N, Term, Arg),
	common_predecessor(Arg, Gvars, ComPred, ComPred1),
	N1 is N-1,
	common_predecessor(N1, Term, Gvars, ComPred1, NewComPred).

:- pop_prolog_flag(multi_arity_warnings).

%
%  Determine the sizes of the subterms of a general term.
%

% This predicate must be tested

general_subterm_size(int, Clause, BT, ST, Gvars, PosSet, Term, Size) :-
	functor(Term, Op, N),
	subterm_int(N, Op, Term, BT, ST, Clause, Gvars, PosSet, Size).
general_subterm_size(length, Clause, BT, ST, Gvars, PosSet, Term, Size) :-
	subterm_length(Clause, BT, ST, Gvars, PosSet, Term, Size1),
	addition(Size1, 1, Size).
general_subterm_size(size, Clause, BT, ST, Gvars, PosSet, Term, Size) :-
	functor(Term, _, N),
	subterm_size(N, Clause, BT, ST, Gvars, PosSet, Term, Size1),
	addition(Size1, 1, Size).
general_subterm_size(depth(ChildList), Clause, BT, ST, Gvars, PosSet, Term,
	    Size) :-
	functor(Term, _, N),
	subterm_depth(N, Clause, BT, ST, Gvars, PosSet, ChildList, Term,
	    Size1),
	addition(Size1, 1, Size).

%
%  Determine the sizes of the subterms of a general term under measure int.
%
subterm_int(1, Op, Term, BT, ST, Clause, Gvars, PosSet, Size) :-
	arg(1, Term, Term1),
	general_term_size(int, Clause, BT, ST, Gvars, PosSet, Term1, Size1),
	Size =.. [Op, Size1].
subterm_int(2, Op, Term, BT, ST, Clause, Gvars, PosSet, Size) :-
	arg(1, Term, Term1),
	general_term_size(int, Clause, BT, ST, Gvars, PosSet, Term1, Size1),
	arg(2, Term, Term2),
	general_term_size(int, Clause, BT, ST, Gvars, PosSet, Term2, Size2),
	Size =.. [Op, Size1, Size2].

%
%  Determine the sizes of the subterms of a general term under measure length.
%

% This predicate needs to be tested

subterm_length(_Clause, _BT, _ST, _Gvars, _PosSet, Term, bot) :-
	nonlist(Term),
	!.
subterm_length( Clause,  BT,  ST,  Gvars,  PosSet, [_|T], Size) :-
	general_term_size(length, Clause, BT, ST, Gvars, PosSet, T, Size).

%
%  Determine the sizes of the subterms of a general term under measure size.
%
subterm_size(0, _Clause, _BT, _ST, _Gvars, _PosSet, _Term, 0) :-
	!.
subterm_size(N,  Clause,  BT,  ST,  Gvars,  PosSet,  Term, Size) :-
	N > 0,
	arg(N, Term, TermN),
	general_term_size(size, Clause, BT, ST, Gvars, PosSet, TermN, SizeN),
	N1 is N-1,
	subterm_size(N1, Clause, BT, ST, Gvars, PosSet, Term, SizeN1),
	addition(SizeN, SizeN1, Size).

%
%  Determine the sizes of the subterms of a general term under measure depth.
%
subterm_depth(0, _Clause, _BT, _ST, _Gvars, _PosSet, _ChildList, _Term, 0) :-
	!.
subterm_depth(N,  Clause,  BT,  ST,  Gvars,  PosSet,  ChildList,  Term,
	    Size) :-
	N > 0,
	(
	    utility:member(ChildList, N) ->
	    (
		arg(N, Term, TermN),
		general_term_size(depth(ChildList), Clause, BT, ST, Gvars,
		    PosSet, TermN, SizeN)
	    )
	;
	    SizeN = 0
	),
	N1 is N - 1,
	subterm_depth(N1, Clause, BT, ST, Gvars, PosSet, ChildList, Term,
	    SizeN1),
	maximum(SizeN, SizeN1, Size).
