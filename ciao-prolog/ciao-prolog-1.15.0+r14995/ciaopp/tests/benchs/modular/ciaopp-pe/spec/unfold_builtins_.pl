:- module(unfold_builtins_,
	[can_be_evaluated/1,
	 peel_call/2],
	 []).

:- use_package(assertions).

:- use_module('..'(p_unit(assrt_db_)), 
 	[assertion_read/9, 
 	 assertion_body/7]).

:- use_module(library(terms), [copy_args/3]).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(library(terms_check), [instance/2]).

:- doc(can_be_evaluated(Goal), "This predicate succeeds if
     @var(Goal) can be executed at compile-time. This can be useful
     both for specializing and analyzing programs. For this, three
     conditions are required about the execution of the goal: it must
     not contain any side-effect, it has to be sufficiently
     instantiated, and its execution must be finite, i.e., must return
     a finite number of solutions (possibly 0) and fail afterwards.").

can_be_evaluated(!):-!, fail. % cuts cannot be executed at analysis time
can_be_evaluated(Goal):-
	no_side_effects(Goal),
	is_evaluable(Goal).


is_evaluable(Goal):-
	assertion_read(Goal,_M,_Status,comp,Body,_VarNames,_S,_LB,_LE),
	assertion_body(Goal,_Compat,Call,_Succ,Comp,_Comm,Body),
	member('basic_props:eval'(_),Comp),
	execute(Call).


execute([]).
execute([Prop|Props]):-
	copy_term(Prop,NProp),
	'$meta_call'(NProp),!,
	instance(Prop,NProp),
	execute(Props).

no_side_effects(Goal):-
	assertion_read(Goal,_M,_Status,comp,Body,_VarNames,_S,_LB,_LE),
	assertion_body(Goal,_Compat,Call,_Succ,Comp,_Comm,Body),
	member('basic_props:sideff'(Goal,free),Comp),
	execute(Call).

%% remove_module_qualification(Goal,PGoal):-
%% 	functor(Goal,F,A),
%% 	peel_call(F,PF),
%% 	functor(PGoal,PF,A),
%% 	copy_args(A,Goal,PGoal).
%% 
peel_call(Goal,PGoal):-
 	atom_concat(_,Post,Goal),
 	atom_concat(':',PGoal,Post),!.

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+414,2004/04/04,18:45*07+'CEST'), "Checking
   preconditions of assertions (to ensure they are applicable) now
   done using '$meta_call'/1.  (German Puebla)").

:- doc(version(1*0+371,2004/03/15,11:56*51+'CET'), "New
   version. Seems to be what we want. It requires the addition of
   @tt{eval} assertions to builtins.  (German Puebla)").

:- doc(version(1*0+363,2004/03/03,19:27*08+'CET'), "First
   version. Uses assertions for describing builtins. Novel assertions
   may be required.  (German Puebla)").

