:- module(p_unit_,
	[ preprocessing_unit/3,
	  program/2,
	  read_program/2,
	  update_program/2,
	  replace_program/2,
	%
	  entry_assertion/3,
	  exit_assertion/3,
	  native_prop/2,
	  native_props/2,
	  type_of_goal/2,
	  type_of_directive/2,
	%
	  new_internal_predicate/3,
	  new_predicate/3,
	  internal_predicate_names/1,
	  predicate_names/1,
	%
	  language/1,
%%jcf-begin
	  internal_pred_name/3,
	  native/2,
	  regtype/2,
	  init_native_props/0,
	  init_types/0,
	  normalize_clauses/1,
	  curr_module/1
%%jcf-end
	],
	[ assertions, basicmodes, regtypes ]).

% Documentation
:- use_module(library(assertions(c_itf_props))).

:- reexport( p_canonical_ ).

:- use_module(library(messages)).

% Ciao library
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(assertions(assertions_props)), [assrt_status/1]).
:- use_module(library(compiler(c_itf)), 
	[ defines_module/2,
	  imports_pred/7,
	  exports/5,
	  def_multifile/4,
	  defines/5,
	  opt_suffix/2,
	  set_ciaopp_expansion/1
	]).
:- use_module(library(lists), [length/2]).
:- use_module(library(vndict), 
	[ create_dict/2, complete_dict/3,
	  varnamedict/1, varnamesl2dict/2
	]).

% CiaoPP library
:- use_module(assrt_db_, [assertion_read/9, assertion_body/7]).
:- use_module(clause_db_).
:- use_module(clidlist_, 
	[ clause/1,clause_key/2,cleanup_clidlist/0,rewrite_source_clause/4]).
:- use_module(unexpand_, [unexpand_meta_calls/2]).
:- use_module(itf_db_, [assert_itf/5, current_itf/3]).
:- use_module(native_, 
	[ builtin/2,
	  builtin_package/1,
	  native_builtin/2,
	  native_prop_map/3,
	  native_prop_term/1,
	  native_property/2
	]).
:- use_module(p_asr_, [preprocessing_unit/3]).
:- use_module('..'(tr_syntax(tr_syntax_)), [cleanup_tr_syntax/0, traverse_clauses/5]).
%% :- use_module(typeslib(typeslib),
%% 	[ legal_user_type_pred/1, 
%% 	  insert_user_type_pred_def/2,
%% 	  remove_parametric_types_from_rules/0,
%% 	  rule_type_symbol/1,
%% 	  simplify_step1/0
%% 	]).

:- on_abort(set_ciaopp_expansion(false)).

:- initialization(opt_suffix(_,'')).

%% ---------------------------------------------------------------------------
:- doc(title,"Preprocessing Unit Information Server").

:- doc(module,"This module loads the preprocessing unit of a file
	to be preprocessed and serves all the information related to it
	to the rest of CiaoPP, including (but not limited to) the source 
	code.").

:- doc(bug,"1. When update_program the relative clause order should be
	maintained: maybe the current clkeys are enough for restoring the
	order upon access."). 
:- doc(bug,"2. May clauses not of the current module be erased?.").
:- doc(bug,"3. Allow for native(regtype).").
:- doc(bug,"4. Properties native and regtype must now be used in assertions
	with a Pred which is a most general goal.").
:- doc(bug,"5. Every component creating new predicates should use 
	new_predicate here. Check.").
% Done when collecting them:
% :- doc(bug,"6. The calls assertion should be unique.").
:- doc(bug,"7. Builtin tables should be generated off-line!.").
% Seems ok now:
% :- doc(bug,"8. There might be info missing for imported predicates; e.g.,
% 	the meta-predicate declarations.").
:- doc(bug,"9. Avoid the need for several native declarations. E.g.,
        for prop var(A) + ( native(free(A)), native(var(A)) ).").
:- doc(bug,"10. Meta terms should not be here.").
%:- doc(bug,"11. visible is not working").
:- doc(bug,"12. Type symbols is not what we want in native_prop").
:- doc(bug,"13. language/1 is a bit naive.").
:- doc(bug,"14. Put make_prop_visible/1 to work.").
:- doc(bug,"15. At least \+ (maybe others?) is not expanded properly.
        See the current chapuza in type_of_goal(imported,_) and
        type_of_goal(metapred,_). This shows up in, e.g., peephole.pl").
:- doc(bug,"16. Have a look to expansion of basic types. Things are rather
	strange now:
                     typedef('basic_props:num',[num]).
                     typedef('arit:arithexpression',[num,['basic_props:num']]).
               ").

%% ---------------------------------------------------------------------------
:- pred preprocessing_unit(I,M,E) : filename(I) => ( moddesc(M), switch(E) )
	# "Loads the preprocessing unit of @var{I} defining @var{M}.".

:- pred preprocessing_unit(Is,Ms,E) : list(filename(Is)) => ( list(moddesc(Ms)), switch(E) )
	# "Loads the preprocessing unit of @var{Is} defining @var{Ms}.".

preprocessing_unit([I|Is],[M|Ms],E):-
	!,
	preprocessing_unit_list([I|Is],[M|Ms],E).
preprocessing_unit(I,M,E):-
	preprocessing_unit_list([I],[M],E).

:- data curr_module/1.

preprocessing_unit_list(Is,Ms,E):-
	set_ciaopp_expansion(true),
	retractall_fact(curr_module(_)),
	retractall_fact(internal_pred_name(_,_,_)),
	retractall_fact(p_unit:native(_,_)),
	retractall_fact(regtype(_,_)),
	p_asr:preprocessing_unit(Is,Ms,E),
	% split pred assertions into calls and success
	% DTM: --- Is this necessary? I _think_ it is done p_asr!
	current_module_to_simple_assrt,
	% identify and assert native props
	init_native_props,
	% setup type definitions
        init_types,
%        remove_parametric_types_from_rules,
%	simplify_step1,
	% remove disjunctions and all that stuff
	cleanup_clidlist,
	cleanup_tr_syntax,
	assert_curr_modules(Ms),
	normalize_clauses_(Ms),
	set_ciaopp_expansion(false).
preprocessing_unit_list(_I,_M,_E):-
	set_ciaopp_expansion(false),
	fail.

%% ---------------------------------------------------------------------------

assert_curr_modules([]).
assert_curr_modules([M|Ms]):-
	asserta_fact(curr_module(M)),
	assert_curr_modules(Ms).

normalize_clauses_([]).
normalize_clauses_([M|Ms]):-
	normalize_clauses(M),
	normalize_clauses_(Ms).

%% ---------------------------------------------------------------------------

init_types:-
        native_prop(Head,regtype(Prop)),
%	displayq( init_types( Head , Prop ) ),nl,
	% definable (i.e., not basic --top, num, etc.-- not [] nor [_|_])
%	legal_user_type_pred(Head),
	( Head==Prop
        ->  findall( (Head:-Body), 
	            ( one_type_clause(Head,Body0),
		      unexpand_meta_calls(Body0,Body)
		    ), Cls )
	 ; Cls=[(Head:-Prop)]
	),
%	displayq( before_insert( Head , Cls ) ),nl,
        ( Cls=[] -> true
        ; 
	    %insert_user_type_pred_def(Head,Cls)
	    true
        ),
%         display(regtype(Head,Prop,Cls)), nl,
        fail.
init_types.

one_type_clause(Head,Body):-
	current_fact(clause_read(_,Head,Body,_VarNames,_Source,_LB,_LE)).
%	!. 
% PP: since there exist type definitions with multiple-clauses
one_type_clause(Head,Body):-
	current_fact(prop_clause_read(_,Head,Body,_VarNames,_Source,_LB,_LE)).

%% ---------------------------------------------------------------------------
:- pred program(P,D) : var * var => list(clause) * varnamedict
	# "@var{P} are the clauses (no directives) of the current module
	   and @var{D} their dictionaries.".

program(P,D):-
	findall((clause(H,B),Key,Dict),
	         current_fact(source_clause(Key,clause(H,B),Dict)),
	        P0),
	split1(P0,P,D).

:- pred read_program(P,D) : var * var => list(clause) * varnamedict
	# "@var{P} are the clauses and directives of the current module
	   and @var{D} their dictionaries.".

read_program(P,D):-
	findall((Clause,Key,Dict),
	         current_fact(source_clause(Key,Clause,Dict)),
	        P0),
	split1(P0,P,D).

split1([],[],[]).
split1([(Cl,K,D)|P0s],[Cl:K|Ps],[D|Ds]):-
	split1(P0s,Ps,Ds).

normalize_clauses(M):-
	findall((Cl,Key,Dict),program_clause(M,Cl,Key,Dict),P0),
	split(P0,Cls0,Ds0),
	traverse_clauses(Cls0,Ds0,all,Cls1,Ds1),
	traverse_clauses(Cls1,Ds1,all,Cls,Ds),
	assert_program(Cls,Ds).

program_clause(M,Cl,Key,Dict):-
	retract_fact(clause_read(M,Head,Body,VarNames,Source,LB,LE)),
	( number(Head)
	-> Cl=directive(Body)
	
	 ; Cl=clause(Head,Body)
	),
	% create a clause id and a reference to program source
	clause_key(Head,Key),
	clause_locator(Key,loc(Source,LB,LE)),
	% El diccionario de variables de c_itf no esta completo!!
	( VarNames=[]
	-> create_dict((Head,Body),Dict)
	 ; varnamesl2dict(VarNames,Dict0),
	   complete_dict(Dict0,(Head,Body),Dict)
	).




split([],[],[]).
split([(Cl,K,D)|P0s],[Cl:K|Ps],[D|Ds]):-
	split(P0s,Ps,Ds).

assert_program([],[]).
assert_program([Cl:Key|Cls],[D|Ds]):-
	clause_format(Cl,H,B),
	rewrite_source_clause(H,B,Key,Clause),
	assertz_fact(source_clause(Key,Clause,D)),
	assert_program(Cls,Ds).

clause_format(clause(H,B),H,B).
clause_format(directive(B),0,B).

:- pred update_program(P,D) : list(clause) * varnamedict
	# "The database holding the program is updated with the clauses in
	   @var{P} and dictionaries in @var{D}. For each one, if there is
	   a clause in the database with same Key, it is replaced; if there
	   is not, it is added. Clauses of the form @tt{delete:Key} cause
	   deletion of the clause identified by @tt{Key} from the database.".

update_program([],[]).
update_program([Cl|Cls],[D|Ds]):-
	update_clause(Cl,D),
	update_program(Cls,Ds).

update_clause(delete:Key,_):- !,
	retract_fact(source_clause(Key,_,_)).
update_clause(Clause:Key,D):-
	( retract_fact(source_clause(Key,_,_)) -> true ; true ),
	assertz_fact(source_clause(Key,Clause,D)).

:- pred replace_program(P,D) : list(clause) * varnamedict # "The
   database holding the program is updated by first deleting its
   contents and then adding the clauses in @var{P} and dictionaries in
   @var{D}.".

replace_program(Cls,Ds):-
	retractall_fact(source_clause(_,_,_)),
	add_all_clauses(Cls,Ds).

add_all_clauses([],[]).
add_all_clauses([Clause:Key|Cls],[D|Ds]):-
	assertz_fact(source_clause(Key,Clause,D)),
	add_all_clauses(Cls,Ds).

%% ---------------------------------------------------------------------------

:- doc(hide,language/1).

language(clp):- 
	current_fact(source_clause(_Key,directive(impl_defined('.=.'/2)),_D)),
	!.
language(lp).

%% ---------------------------------------------------------------------------
:- pred entry_assertion(Goal,Call,Name) :: callable(Goal)
	# "There is an entry assertion for @var{Goal} with call
           pattern @var{Call}, uniquely identifiable by @var{Name}.".

entry_assertion(Goal,Call,Name):-
	curr_module(M),
	current_fact(
		assertion_read(Goal,M,_Status,entry,Body,_Dict,_S,_LB,_LE) ),
	assertion_body(Goal,_Compat,Call,_Succ,Comp,_Comm,Body),
	( builtin(entry_point_name(Goal,Name),Entry),
	  member(Entry,Comp)
	-> true
	; functor(Goal,Name,_)
	).

%% ---------------------------------------------------------------------------
:- pred exit_assertion(Goal,Call,Succ) :: callable(Goal)
	# "There is an exit assertion for @var{Goal} with call
           pattern @var{Call} and success pattern @var{Succ}.".

exit_assertion(Goal,Call,Succ):-
	( Type=pred ; Type=success ),
	( Status=true ; Status=trust ),
	current_fact(
		assertion_read(Goal,_M,Status,Type,Body,_Dict,_S,_LB,_LE) ),
	assertion_body(Goal,_Compat,Call,Succ,_Comp,_Comm,Body).

%% ---------------------------------------------------------------------------
:- pred type_of_goal(Type,Goal) :: ( callable(Goal),
	member(Type,[imported, exported, multifile, dynamic,
	             metapred(T,Meta), builtin(T)]) )
        # "@var{Goal} is declared of type @var{Type}.".

% This one can be optimized depending on the calling mode!
type_of_goal(imported,Goal):-
	current_itf(imports,Goal,_).
type_of_goal(imported,\+(_)). %otra chapuza mas mientras Ciao-1.11 no funcione
type_of_goal(imported(M),Goal):-
	current_itf(imports,Goal,M).
type_of_goal(exported,Goal):-
	current_itf(exports,Goal,_).
type_of_goal(multifile,Goal):-
	current_itf(multifile,Goal,_).
type_of_goal(dynamic,Goal):-
	current_itf(dynamic,Goal,_).
% these might be defined outside:
type_of_goal(metapred(call(X),call(goal)),call(X)).
type_of_goal(metapred(not(X),\+(goal)),\+(X)).
type_of_goal(metapred(if(X,Y,Z),if(goal,goal,goal)),if(X,Y,Z)).
%
type_of_goal(metapred(apply(X,Y),call(goal,?)),call(X,Y)).
type_of_goal(metapred(apply(X,Args),Meta),Goal):-
	functor(Goal,call,A),
	A > 2,
	Goal=..[call,X|Args],
	A1 is A-1,
	length(L,A1),
	list(L,=(?)),
	Meta=..[call,goal|L].
type_of_goal(metapred(Type,Meta),Goal):-
	current_itf(meta,Goal,Meta),
	( type_of_goal(builtin(Type),Goal)
	-> true 
	 ; Type=Goal
	).
% the one introduced by prepare_ai_output:
type_of_goal(metapred(true(G),true(goal)),true(G)).
% DTM: Neccesary for rtchecks (and correct unexpand)
% type_of_goal(metapred(check(X),check(goal)),check(X)).
type_of_goal( metapred('rtchecks_mod:check'(X),
	               'rtchecks_mod:check'(goal)),
		       'rtchecks_mod:check'(X)).
type_of_goal( metapred('rtchecks_mod:checkc'(X,Y),
	               'rtchecks_mod:checkc'(goal,?)),
		       'rtchecks_mod:checkc'(X,Y)).
type_of_goal( metapred('rtchecks_mod:checkiftrue'(X,Y),
	               'rtchecks_mod:checkiftrue'(?,goal)),
		       'rtchecks_mod:checkiftrue'(X,Y)).

type_of_goal( imported( library( rtchecks_mod ) ),
	      'rtchecks_mod:check'(_) ).
type_of_goal( imported( library( rtchecks_mod ) ), 
	      'rtchecks_mod:checkc'(_,_) ).
type_of_goal( imported( library( rtchecks_mod ) ), 
	      'rtchecks_mod:checkiftrue'(_,_) ).

%
type_of_goal(builtin(Type),Goal):-
	native_builtin(Goal,Type), !.  % builtin tables
type_of_goal(builtin(Type),Goal):-
	current_fact(
		assertion_read(Goal,_M,_Status,_Type,Body,_Dict,_S,_LB,_LE) ),
	assertion_body(Goal,_Compat,_Call,_Succ,Comp,_Comm,Body),
	builtin(native(Goal,Type),Native),
	member(Native,Comp).

%% ---------------------------------------------------------------------------
:- pred type_of_directive(Type,Body) 
	# "There is a directive of the form @var{:- Type Body}
           (of arity one).".

type_of_directive(Type,Body):-
	functor(D,Type,1),
	arg(1,D,Body),
	current_fact(
        clause_read(_M,0,D,_VarNames,_Source,_LB,_LE) ).

%% ---------------------------------------------------------------------------

:- pred new_predicate(+F,+A,-NewF)
	# "Checks wether there is a predicate @var{F}/@var{A} in the
           program and returns @var{NewF} so that there is no predicate
           @var{NewF}/@var{A} in the program.".

new_predicate(F,A,NewF):-
	curr_module(M),
	new_predicate_name(F,F,A,0,NewF),
	assert_itf(defined,M,NewF,A,_).
	
new_predicate_name(TmpF,F,A,N,NewF):-
	current_itf(visible,TmpF,A), !,
	N1 is N+1,
	name(N1,S1),
	"_"=[S],
	atom_codes(Suffix,[S|S1]),
	atom_concat(F,Suffix,TmpF1),
	new_predicate_name(TmpF1,F,A,N1,NewF).
new_predicate_name(NewF,_F,_A,_N,NewF).

:- pred predicate_names(-list)
	# "Returns the specs of predicates defined in the current module.".

predicate_names(Names):-
	findall(F/A, current_itf(defines,F,A), Names).

:- data internal_pred_name/3.

:- doc(hide,internal_predicate_names/1).

internal_predicate_names(Names):-
	findall((F,A,NF), current_fact(internal_pred_name(F,A,NF)), Names).

:- doc(hide,new_internal_predicate/3).

% this checks clashes with current module predicates, but not between
% internal names (which are names created by CiaoPP, not module-expanded)
new_internal_predicate(F,A,NewF):-
	current_fact(internal_pred_name(F,A,NewF)), !.
new_internal_predicate(F,A,NewF):-
	(
	    curr_module( user(_) )
	->  
	    M = user
	;
	    curr_module(M)
	),
	atom_concat(M,:,Mm),
	atom_concat(Mm,F,MF),
	new_predicate_name(MF,MF,A,0,NewF0),
	( MF==NewF0 -> 
	  NewF=F
	; NewF=NewF0,
	  asserta_fact(internal_pred_name(F,A,NewF))
	).

%% ---------------------------------------------------------------------------

:- redefining(native/2). % also in basic_props
:- data native/2, regtype/2.

:- pred native_prop(Goal,Prop) :: callable * native_prop_term
      # "@var{Goal} is an atom of a predicate which
	corresponds to the native property (atom) @var{Prop}.".

native_prop(Goal,regtype(Prop)):-
	current_fact(regtype(Goal,Prop0)),
	( native_prop_(Goal,Prop) -> true
	; Prop=Prop0
	).
native_prop(Goal,Prop):-
	native_prop_(Goal,Prop).

native_prop_(Goal,Prop):-
	current_fact(p_unit:native(Goal,Prop)).
native_prop_(Goal,Prop):-
	native_property(Goal,Prop). % builtin tables

init_native_props:-
%Nop!	current_itf(visible,Goal,_),
	current_fact( % only prop assertions
		assertion_read(Goal,_M,_,prop,Body,_,_,_,_) ),
	assertion_body(Goal,_Compat,_Call,_Succ,Comp,_Comm,Body),
	% should assert most general goals?
	% can be native several times
	( builtin(native(Goal,Prop),Native),
	  member(Native,Comp),
%	  displayq( native_props( Goal , Prop ) ), nl,
	  asserta_fact(p_unit:native(Goal,Prop))
	 ; true
	),
	% can be regtype only once
	( builtin(regtype(Prop0),Regtype),
	  member(Regtype,Comp)
	-> unexpand_meta_calls(Prop0,Type),
%	  displayq( regtype( Goal , Prop ) ), nl,
	   asserta_fact(regtype(Goal,Type))
	 ; true
	),
	fail.
%% init_native_props:-
%%         current_fact(regtype(Head,Prop)),
%%         display(regtype(Head,Prop)), nl,
%% 	fail.
init_native_props.

%% ---------------------------------------------------------------------------

:- pred native_props(Props,Goals) :: list(callable) * list(callable)
	# "Maps native @var{Props} into their corresponding @var{Goals}
	  visible in the current module.".

native_props([],[]).
native_props([I|Info],OutputUser):-
	do_native_prop(I,OutputUser,OutputUser1),
	native_props(Info,OutputUser1).

do_native_prop(Prop,OutputUser,OutputUser1):-
	native_prop_map(Prop,P,Vars), !,
	do_for_each(Vars,P,OutputUser,OutputUser1).
do_native_prop(Prop,[O|OutputUser],OutputUser):-
	do_native_prop_(Prop,O).

do_native_prop_(I,O):-
	native_prop(O,I),
	current_itf(visible,O,_), !.
do_native_prop_(I,O):-
	native_property(O,I). % builtin tables
% should be:
%% do_native_prop_(I,O):-
%% 	native_prop(O,I), !,
%% 	make_prop_visible(O).
do_native_prop_(I,I).
/*
% TYPE_SYMBOLS_NOT_WHAT_WE_WANT
do_native_prop_(I,O):-
	functor(I,T,1),
% not really: should check that it is indeed a type!!!
	rule_type_symbol(T), !,
	O=I.
do_native_prop_(I,I):-
	curr_module(M),
	builtin_package(B),
	( current_fact( clause_read(M,0,use_package(B),_,_Source,_LB,_LE) )
	-> true
	 ; assertz_fact( clause_read(M,0,use_package(B),no,0,0,0) )
	).
*/

do_for_each([V|Vars],P,[O|OutputUser],OutputUser1):-
	functor(Prop,P,1),
	arg(1,Prop,V),
	do_native_prop_(Prop,O),
	do_for_each(Vars,P,OutputUser,OutputUser1).
do_for_each([],_P,OutputUser,OutputUser).

/*
make_prop_visible(O):-
	functor(O,F,A),
	extract_module(F,M),
	module_spec(Spec,M), % if it was reversible!
	functor(G,F,A),
	assert_itf_chapuza(p_unit,imports(F,Spec)).
*/

%% ---------------------------------------------------------------------------

:- doc(version_maintenance,dir('../version/')).

:- doc(version(1*0+697,2004/10/07,19:05*10+'CEST'), " Removed cut in
   one_type_clause/2 - it has to be non-deterministic. Now,
   user-defined types can have more than 1 rule.  (Pawel Pietrzak)").

:- doc(version(1*0+615,2004/09/09,17:38*11+'CEST'), "Removed
   redundant code in preprocessing_unit/3 (Jesus Correas Fernandez)").

:- doc(version(1*0+583,2004/07/26,13:51*54+'CEST'), "Added in
   preprocessing_unit/3 code for handling a list of current modules.
   (Jesus Correas Fernandez)").

:- doc(version(1*0+563,2004/07/17,02:00*05+'CEST'), "Fixed
           tr_syntax bug with brutal force.  (David Trallero Mena)").

:- doc(version(1*0+559,2004/07/16,17:58*44+'CEST'),
           "normalizar_assertions predicate moved to p_canonical
           module.  (David Trallero Mena)").

:- doc(version(1*0+525,2004/07/07,10:43*30+'CEST'), "added
   messages to allow to use error_message (David Trallero Mena)").

:- doc(version(1*0+524,2004/07/05,18:58*37+'CEST'), "added
   decide_status (David Trallero Mena)").

:- doc(version(1*0+500,2004/07/02,13:27*19+'CEST'), "Added
   metrapred in type_of_goal for correct unexpand when printing output
   Corrected a bug in bug numbers

    (David Trallero Mena)").

:- doc(version(1*0+357,2004/02/25,19:20*34+'CET'), "Some internal
   predicates exported to be used from m_unit.pl.  (Jesus Correas
   Fernandez)").

:- doc(version(1*0+172,2003/12/29,10:24*10+'CET'), "Properties
   native and regtype can only be asserted in prop assertions.
   (Francisco Bueno Carrillo)").

:- doc(version(1*0+167,2003/12/26,13:50*29+'CET'), "Complete
   clause dicts, which do not have names for the clause free
   variables.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+162,2003/12/23,20:43*51+'CET'), "Do not treat
   prop assertions as pred assertions.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+133,2003/10/15,03:55*37+'CEST'), "Changed
   management of regtype/1 in native_prop/2.  (Francisco Bueno
   Carrillo)").

:- doc(version(1*0+94,2003/10/03,15:19*30+'CEST'),
   "new_internal_predicate failed when no ':- module' declaration was used.
   (David Trallero Mena)").

:- doc(version(1*0+91,2003/09/18,22:14*21+'CEST'), "Added
   new_internal_predicate/3 and internal_predicate_names/1.
   (Francisco Bueno Carrillo)").

:- doc(version(1*0+73,2003/09/12,12:03*53+'CEST'), "Meta-pred
   decls. for imported now asserted and correctly expanded.
   (Francisco Bueno Carrillo)").

:- doc(version(1*0+72,2003/09/12,12:03*04+'CEST'), "Used visible
   in new_predicate.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+68,2003/09/11,18:21*26+'CEST'), "Adapted
   native_props/2 to check visibility and native_prop/2 to not check
   it.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+67,2003/09/11,12:20*04+'CEST'), "Added
   language/1.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+61,2003/09/08,19:31*53+'CEST'), "Now
   itf(visible) seems to be working.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+50,2003/09/01,18:24*07+'CEST'), "Do not write
   comp or succ assertion for a pred assertion if there is no info.
   (Francisco Bueno Carrillo)").

:- doc(version(1*0+39,2003/08/28,16:09*40+'CEST'), "Added
   predicate_names/1.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+36,2003/08/27,13:17*46+'CEST'), "Add a comp
   (besides the success) assertion for each pred assertion. (Francisco
   Bueno Carrillo)").

:- doc(version(1*0+2,2002/01/11,18:07*27+'CET'), "Final version
   (for now) of the preprocessing unit.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+1,2001/11/07,19:05*57+'CET'), "First shot at
   reading a complete preprocessing unit for the current module.
   (Francisco Bueno Carrillo)").

%% ---------------------------------------------------------------------------
