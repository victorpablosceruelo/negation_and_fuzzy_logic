:- module(hmtypes_check_rt, [], [assertions]).
:- use_package(hiord). % required by httypes_check_common
:- include(library(hmtypes_check(hmtypes_check_ops))).

:- use_module(library(format), [format/2]).

:- include(library(hmtypes_check(hmtypes_check_runtime_db))).

% ---------------------------------------------------------------------------
% (Run time) instance of the type check algorithm

:- export(type_check_term/5).
:- export(equate_types/3).
:- export(term_type_error/4).
:- export(numeric_type/2).

:- include(library(hmtypes_check(hmtypes_check_common))).
:- include(library(hmtypes_check(functor_constraint))).
hmt__constructor(Term, ExpectedType, Mod, EnvIn, EnvOut) :-
	'$hmt__constructor'(Term, ExpectedType, Mod, EnvIn, EnvOut).
hmt__constructor_info(Term, Type, Args, Types, Mod) :-
	'$hmt__constructor_info'(Term, Type, Args, Types, Mod).
hmt__signature(Call, Types, Mod, EnvIn, EnvOut) :-
	'$hmt__signature'(Call, Types, Mod, EnvIn, EnvOut).
hmt__basic_normalize(Alias, Type, Mod) :-
	'$hmt__basic_normalize'(Alias, Type, Mod).

% TODO: Add pretty print for this exception
% prolog:message(error(type_error(Term,ExpectedType,InferredType,_Mod))) -->
% 	[ '\n\tTYPE ERROR: expected type `~w\' for term `~w\'\n' - [ExpectedType, Term],
% 	  '\t            inferred type `~w\'\n' - [InferredType]
% 	].

