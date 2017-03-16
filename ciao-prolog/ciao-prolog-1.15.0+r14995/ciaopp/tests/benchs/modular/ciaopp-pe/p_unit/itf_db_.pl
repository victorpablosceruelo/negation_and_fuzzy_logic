:- module(itf_db_,
	[ assert_itf/5, assert_itf_chapuza/2,
	  cleanup_itf_db/0,
	  curr_file/2, current_itf/3,
	  retract_itf/5
	],
	[ assertions ] ).

:- use_module(library(compiler(c_itf)), [module_expansion/9]).

:- use_module(unexpand_, [unexpand_meta_calls/2]).

:- data defines/2, imports/2, exports/1, multifile/2, meta/2, dynamic/1.
:- data curr_file/2.

cleanup_itf_db:-
	retractall_fact(defines(_,_)),
	retractall_fact(imports(_,_)),
	retractall_fact(exports(_)),
	retractall_fact(multifile(_,_)),
	retractall_fact(meta(_,_)),
	retractall_fact(dynamic(_)),
	retractall_fact(curr_file(_,_)).

assert_itf(defined,_M,F,A,_Type):- % already expanded
	asserta_fact(defines(F,A)).
assert_itf(defines,M,F,A,_Type):-
	functor(Goal0,F,A),
	c_itf:module_expansion(Goal0,true,M,_Dict,asr,_,_,Goal,_Body),
	functor(Goal,FG,A),
	asserta_fact(defines(FG,A)).
assert_itf(imports,M,F,A,IM):-
	functor(Goal0,F,A),
	c_itf:module_expansion(true,Goal0,M,_Dict,asr,_,_,_Body,Goal1),
	unexpand_meta_calls(Goal1,Goal),
	asserta_fact(imports(Goal,IM)).
assert_itf(exports,M,F,A,_M):-
	functor(Goal0,F,A),
	c_itf:module_expansion(Goal0,true,M,_Dict,asr,_,_,Goal,_Body),
	asserta_fact(exports(Goal)).
assert_itf(new_exports,_M0,F,A,_M):-
	functor(Goal,F,A),
	asserta_fact(exports(Goal)).
assert_itf(multifile,M,F,A,_DynType):-
	functor(Goal0,F,A),
	c_itf:module_expansion(Goal0,true,M,_Dict,asr,_,_,Goal,_Body),
	asserta_fact(multifile(Goal,M)).
assert_itf(meta,M,F,A,Meta0):-
	functor(Goal0,F,A),
	c_itf:module_expansion(true,Goal0,M,_DictA,asr,_,_,_BodyA,Goal),
	c_itf:module_expansion(true,Meta0,M,_DictB,asr,_,_,_BodyB,Meta),
	asserta_fact(meta(Goal,Meta)).
assert_itf(dynamic,M,F,A,_Deftype):-
	functor(Goal0,F,A),
	c_itf:module_expansion(Goal0,true,M,_Dict,asr,_,_,Goal,_Body),
	asserta_fact(dynamic(Goal)).
%jcf%
assert_itf(defines_module,M,_,_,Base):-
	( current_fact(defines_module(Base,M)) ->
	  true
	; asserta_fact(defines_module(Base,M))).
:- data defines_module/2.
%jcf%

assert_itf_chapuza(remote,imports(Goal,IM)):-
	( current_fact(imports(Goal,IM)) -> true
	; asserta_fact(imports(Goal,IM)) ).

:- pred retract_itf(+Class,_M0,+F,+A,_M)
	# "This predicate allows removing itf information when it 
           is no longer true. This can happen for example during 
           program transformation.". 

retract_itf(exports,_M0,F,A,_M):-
	functor(Goal,F,A),
	retract_fact(exports(Goal)).

current_itf(visible,Goal,X):-
	var(X),
	visible_goal(Goal).
current_itf(visible,F,A):-
	nonvar(A),
	visible_spec(F,A).
current_itf(defines,F,A):-
	current_fact(defines(F,A)).
current_itf(imports,Goal,IM):-
	current_fact(imports(Goal,IM)).
current_itf(exports,Goal,_M):-
	current_fact(exports(Goal)).
current_itf(exports,Goal,_M):-
	current_fact(curr_file(_,user(_))),
	current_fact(defines(F,A)),
	functor(Goal,F,A).
current_itf(multifile,Goal,M):-
	current_fact(multifile(Goal,M)).
current_itf(meta,Goal,Meta):-
	current_fact(meta(Goal,Meta)).
current_itf(dynamic,Goal,_Deftype):-
	current_fact(dynamic(Goal)).
%jcf%
current_itf(defines_module,M,Base):-
	current_fact(defines_module(Base,M)).
%jcf%

visible_goal(Goal):-
	current_itf(imports,Goal,_).
visible_goal(Goal):-
	current_itf(defines,F,A),
	functor(Goal,F,A).
visible_goal(Goal):-
	current_fact(multifile(Goal,_)).

visible_spec(F,A):-
	current_itf(defines,F,A).
visible_spec(F,A):-
	current_itf(imports,Goal,_),
	functor(Goal,F,A).
visible_spec(F,A):-
	current_fact(multifile(Goal,_)),
	functor(Goal,F,A).

% -----------------------------------------------------------------------
:- doc(version_maintenance,dir('../version/')).

:- doc(version(1*0+694,2004/10/07,18:05*44+'CEST'), "Added module
   to multifile predicates information.    (Jesus Correas)").

:- doc(version(1*0+682,2004/10/02,09:18*17+'UTC'), "Added exported
   predicate @pred{retract_itf/5}.  (German Puebla)").

:- doc(version(1*0+280,2004/02/02,18:38*41+'CET'), "Double module
   expansion for meta-predicates to solve the problem with addmodule
   specs.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+252,2004/01/29,15:05*30+'CET'), "All predicates
   defined in a user file are considered exports.  (Francisco Bueno
   Carrillo)").

:- doc(version(1*0+84,2003/09/15,13:20*55+'CEST'), "Added
   assert_itf_chapuza/2.  (Francisco Bueno Carrillo)").
