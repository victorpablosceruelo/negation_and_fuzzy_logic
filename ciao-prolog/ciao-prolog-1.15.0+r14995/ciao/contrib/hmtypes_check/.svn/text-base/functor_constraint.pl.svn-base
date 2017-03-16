% :- module(functor_constraint, [functor_constraint/4], []).
% Note: this is a 'template' code, included from two different modules, 
%   because of hmt__constructor_info.
:- use_package(hiord).
:- use_package(attr). % TODO: Check that it is working

:- use_module(library(terms_vars)).
:- use_module(engine(attributes)).
:- use_module(library(aggregates)).

term_variables(Term, Vs) :- varset(Term, Vs).

functor_constraint(Term,Type,Mod,Args,ArgTypes) :-
	check_propagator(Term,Type,Mod,Args,ArgTypes,Results),
	Results \== [], % no solution
	( Results = [Result] -> % one solution
		Result = constructor_info(Term,Type,Args,ArgTypes)
	; % multiple solutions
		term_variables([Type|ArgTypes],SuspensionVars),
		Closure = functor_constraint_reactivation(Term,Type,Mod,Args,ArgTypes,_KillFlag),
		suspend_functor_constraint(SuspensionVars,Closure)	
	).	

functor_constraint_reactivation(Term,Type,Mod,Args,ArgTypes,KillFlag,Var) :-
	( var(KillFlag) ->
		check_propagator(Term,Type,Mod,Args,ArgTypes,Results),
		Results \== [], % no solution
		( Results = [Result] -> % one solution
			Result = constructor_info(Term,Type,Args,ArgTypes),
			KillFlag = dead
		; % multiple solutions
			% TODO: narrow possibilities for argument types 
			%	using type domain
			( nonvar(Var) -> 
				term_variables(Var,SuspensionVars),
				Closure = functor_constraint_reactivation(Term,Type,Mod,Args,ArgTypes,_KillFlag),
				suspend_functor_constraint(SuspensionVars,Closure)
			;
				true
			)
		)	
	;
		true
	).	

suspend_functor_constraint([], _Closure).
suspend_functor_constraint([V|Vs], Closure) :-
	var_suspend_functor_constraint(Closure, V),
	suspend_functor_constraint(Vs, Closure).

var_suspend_functor_constraint(Closure,Var) :-
	% note: attributes are local to the module in Ciao attr
%	put_attr(Var,functor_constraint,Closure).
	put_attr_local(Var,Closure).

% TODO: Wrong! This hook is included twice! (Remy)
attr_unify_hook(functor_constraint_reactivation(Term,Type,Mod,Args,ArgTypes,KillFlag), Term) :-
	functor_constraint_reactivation(Term,Type,Mod,Args,ArgTypes,KillFlag, Term).

check_propagator(Term,Type,Mod,Args,ArgTypes,Results) :-
	copy_term_nat(propagator(Term,Type,Args,ArgTypes),
	              propagator(TermC,TypeC,ArgsC,ArgTypesC)),
	findall(constructor_info(TermC,TypeC,ArgsC,ArgTypesC),
                hmt__constructor_info(TermC,TypeC,ArgsC,ArgTypesC,Mod),Results).


