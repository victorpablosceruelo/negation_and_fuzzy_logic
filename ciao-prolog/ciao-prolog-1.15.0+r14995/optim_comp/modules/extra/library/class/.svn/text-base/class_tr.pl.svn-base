%------------------------------------------------------------------------
%
% O'Ciao: Object Oriented Programming in Ciao/Prolog
%
% SOURCE-TO-SOURCE EXPANSION FOR CLASS DECLARATION
%
% AUTHORs : Angel Fernandez Pineda (original author)
%           Jose F. Morales (adapted to Ciao the compiler and optimized)
%
% CLIP Laboratory - Technical University Of Madrid.
%
%         - Distributed under the Ciao Prolog license terms -
%
%------------------------------------------------------------------------

:- module(class_tr,
	[
	    class_sentence_trans/3,
	    class_clause_trans/3
	], []).

%%------------------------------------------------------------------------

:- use_module(.(class_old_itf)).
:- use_module(library(lists), [append/3]).
:- use_module(library(class(class_itf))).
:- use_module(library(class(class_error_reporting))).
:- use_module(library(aggregates), [findall/3]).

%%------------------------------------------------------------------------
%% All those facts are indexed by module name(=class name)...

:- data super/2.               % CLASS inherits from SUPER.
:- data inherit_class/2.       % CLASS inherits from SOURCE FileName 
:- data implements/2.          % CLASS implements INTERFACE

:- data initial_state/3.       % CLASS declares that FACT(functor) 
                               % with ARGS must be present 
                               % at instance creation.

:- data is_inheritable/3.      % CLASS declared F/A as inheritable.
:- data is_public/3.           % CLASS declared F/A as public.
:- data is_multifile/3.        % CLASS declared F/A as multifile.
:- data is_attribute/3.        % CLASS declared F/A as data or dynamic.
:- data is_method/3.           % CLASS declared F/A as static (method).
:- data is_virtual/3.          % CLASS declared F/A as a virtual method.
:- data is_concurrent/3.       % CLASS declared F/A as a concurrent attribute.
:- data is_metapredicate/2.    % CLASS declared SPEC as a 
                               % meta_predicate specification.

class_sentence_trans(0,_,Module) :- !,
	initialize_class_trans(Module),
	fail.
% Note: all called goals are assumed not to fail.
%       '$end$$of$$expansion$' clause is used to detect ending of
%       second-pass expansion.
class_sentence_trans(end_of_file,FinalClauses,Module) :- !,
	finish_class_trans(Module, FinalClauses). 
class_sentence_trans((:- Decl), As, Mod) :- !,
	class_treat_decl(Decl, As, Mod).
%% DECLARE INITIAL STATE
class_sentence_trans((Head :- _),[],Mod) :-
	functor(Head,F,A),
	is_attribute(Mod,F,A),
	!,
	error(Mod, initial_state_clause_ignored(F, A)).
class_sentence_trans((Head),[],Mod) :-
	functor(Head,F,A),
	is_attribute(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	Head =.. [_|Args],
	attr_qualify(Mod, F, QfdF),
	assertz_fact(initial_state(Mod,QfdF,Args)),
	fail.
%% CONSTRUCTOR DECLS
class_sentence_trans((Head :- _),_,Module) :-
	functor(Head,Module,Arity),
	add_is_inheritable(Module,Module,Arity),
	fail.
class_sentence_trans((Head),_,Module) :-
	functor(Head,Module,Arity),
	add_is_inheritable(Module,Module,Arity),
	fail.
%% METHOD CLAUSE RECOGNITION
%% (METHOD BODY WILL BE EXPANDED AT SECOND PASS)
class_sentence_trans((Head :- Body),[(NewHead :- Body)],Mod) :-
	functor(Head,F,A),
	\+ is_attribute(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	method_head(Head,_,NewHead),
	add_is_method(Mod, F, A).
class_sentence_trans((Head),[(NewHead)],Mod) :-
	functor(Head,F,A),
	\+ functor(Head,':-',_),
	\+ is_attribute(Mod,F,A),
	\+ is_multifile(Mod,F,A),
	!,
	method_head(Head,_,NewHead),
	add_is_method(Mod, F, A).

add_is_method(Mod, F, A) :-
	( is_method(Mod,F,A) -> true ; asserta_fact(is_method(Mod,F,A)) ).

add_is_inheritable(Mod, F, A) :-
	( is_inheritable(Module,F,Arity) ->
	    true
	; asserta_fact(is_inheritable(Module,F,Arity))
	).

add_is_public(Mod, F, A) :-
	( is_public(Mod,F,A) ->
	    true
	; asserta_fact(is_public(Mod,F,A))
	).

%%------------------------------------------------------------------------
%% INITIALIZATION

initialize_class_trans(Module) :-
	cleanup_class_info(Module),
	retractall_fact(super(Module,_)),
	retractall_fact(inherit_class(Module,_)),
	retractall_fact(implements(Module,_)),
	retractall_fact(initial_state(Module,_,_)),
	retractall_fact(is_inheritable(Module,_,_)),
	retractall_fact(is_public(Module,_,_)),
	retractall_fact(is_method(Module,_,_)),
	retractall_fact(is_virtual(Module,_,_)),
	retractall_fact(is_metapredicate(Module,_)),
	retractall_fact(is_attribute(Module,_,_)),
	retractall_fact(is_multifile(Module,_,_)),
	retractall_fact(is_concurrent(Module,_,_)).

% ------------------------------------------------------------------------
% Declarations

% Normalize multiple-spec declarations
class_treat_decl(IsDecl,Exp,Module) :-
	functor(IsDecl,Decl,1),
	( Decl = public ;
	  Decl = export ;
	  Decl = inheritable ;
	  Decl = data ;
	  Decl = dynamic ;
	  Decl = meta_predicate ;
	  Decl = multifile ;
	  Decl = concurrent ;
	  Decl = discontiguous ;
	  Decl = virtual 
	),
	arg(1,IsDecl,Arg),
	functor(Arg,',',2),
	!,
	sequence_to_list(Arg,List),
	functor(NewDecl,Decl,1),
	arg(1,NewDecl,List),
	class_treat_decl(NewDecl,Exp,Module).
class_treat_decl(export(X),Exp,Mod) :- !,
	class_treat_decl(public(X),Exp,Mod).
class_treat_decl(public([]),[],_) :- !.
class_treat_decl(public([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(public(Spec),OneDeclExp,Mod),
	class_treat_decl(public(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(inheritable([]),[],_) :- !.
class_treat_decl(inheritable([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(inheritable(Spec),OneDeclExp,Mod),
	class_treat_decl(inheritable(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(virtual([]),[],_) :- !.
class_treat_decl(virtual([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(virtual(Spec),OneDeclExp,Mod),
	class_treat_decl(virtual(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(data([]),[],_) :- !.
class_treat_decl(data([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(data(Spec),OneDeclExp,Mod),
	class_treat_decl(data(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(dynamic([]),[],_) :- !.
class_treat_decl(dynamic([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(public(Spec),OneDeclExp,Mod),
	class_treat_decl(public(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(concurrent([]),[],_) :- !.
class_treat_decl(concurrent([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(concurrent(Spec),OneDeclExp,Mod),
	class_treat_decl(concurrent(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(multifile([]),[],_Mod) :- !.
class_treat_decl(multifile([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(multifile(Spec),OneDeclExp,Mod),
	class_treat_decl(multifile(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(meta_predicate([]),[],_) :- !.
class_treat_decl(meta_predicate([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(meta_predicate(Spec),OneDeclExp,Mod),
	class_treat_decl(meta_predicate(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
class_treat_decl(discontiguous([]),[],_) :- !.
class_treat_decl(discontiguous([Spec|Nsp]),Exp,Mod) :- !,
	class_treat_decl(discontiguous(Spec),OneDeclExp,Mod),
	class_treat_decl(discontiguous(Nsp),NExp,Mod),
	append(OneDeclExp,NExp,Exp).
%% INTERFACE IMPLEMENTATION
class_treat_decl(implements(Source),
	[
	    (:- reexport(Source,[]))
	],Mod) :-
	class_from_base(Source,Itf),
	\+ implements(Mod,Itf),
	assertz_fact(implements(Mod,Itf)),
	!.
class_treat_decl(implements(_),[],_Mod) :- !.
%% AVOID RESERVED DECLARATIONS
class_treat_decl(super(_),[],_) :- !.
class_treat_decl(attribute(_),[],_) :- !.
class_treat_decl(method(_),[],_) :- !.
%% INHERITANCE DECLARATION
class_treat_decl(inherit_class(_),[],Module) :-
	super(Module,_),
	!,
	error(Module, multiple_inheritance).
class_treat_decl(inherit_class(SuperSource),
	[
	    (:- inherit_class(SuperSource)),
	    (:- super(Super))
	],Module) :-
	class_from_base(SuperSource,Super),
	asserta_fact(super(Module,Super)),
	asserta_fact(inherit_class(Module,SuperSource)),
	!.
class_treat_decl(inherit_class(_),[],Module) :- !,
	error(Module, invalid_inheritance_decl).
%% KEEP TRACKING OF METAPREDICATE DECLARATIONS
class_treat_decl(meta_predicate(Spec),[],Mod) :-
	Spec =.. [_|SpecArgs],
	functor(Spec,F,A),
	\+ is_multifile(Mod,F,A),
	has_addmodule(SpecArgs),
	!,
	error(Mod, addmodule_not_allowed(F, A)).
class_treat_decl(meta_predicate(Spec),[],Mod) :-
	functor(Spec,F,A),
	\+ is_multifile(Mod,F,A),
	assertz_fact(is_metapredicate(Mod,Spec)),
	!.
%% KEEP TRACKING OF DATA DECLARATIONS
% 1st case: normalize attribute declarations 
class_treat_decl(dynamic(F/A),Exp,Mod) :- !,
	class_treat_decl(data(F/A),Exp,Mod).
% 2nd case: invalid spec.
class_treat_decl(data(F/A),[],Mod) :-
	( (\+ atom(F)) ; (\+ integer(A)) ),
	!,
	error(Mod, invalid_attribute_decl(F, A)).
% 3rd case: already declared as method.
class_treat_decl(data(F/A),[],Mod) :-
	is_method(Mod,F,A),
	!,
	error(Mod, attribute_was_a_method(F, A)).
% 4nd case: already declared.
class_treat_decl(data(F/A),[],Mod) :-
	is_attribute(Mod,F,A),
	!.
% 5rd case: multifile declaration already found.
class_treat_decl(data(F/A),[],Mod) :-
	is_multifile(Mod,F,A),
	!.
% 6th case: predicate is reserved.
class_treat_decl(data(destructor/0),[],Mod) :- !,
	error(Mod, cannot_be_attribute(destructor, 0)).
class_treat_decl(data(Module/A),[],Module) :- !,
	error(Module, cannot_be_attribute(Module, A)).
% 7th case: valid attribute declaration.
class_treat_decl(data(F/A),[],Mod) :-
	asserta_fact(is_attribute(Mod,F,A)),
	!.
%% KEEP TRACKING OF MULTIFILE DECLARATIONS
% 0 case : F/A is a reserved predicate.
class_treat_decl(multifile(destructor/0),[],Mod) :- !,
	error(Mod, invalid_multifile_reserved(destructor, 0)).
class_treat_decl(multifile(Module/A),[],Module) :- !,
	error(Module, invalid_multifile_reserved(Module, A)).
% 1st case : F/A was declared dynamic before multifile
class_treat_decl(multifile(F/A),[],Mod) :-
	is_attribute(Mod,F,A),
	!,
	retract_fact(is_attribute(Mod,F,A)),
	asserta_fact(is_multifile(Mod,F,A)).
% 2nd case : F/A was declared method before multifile
class_treat_decl(multifile(F/A),[],Mod) :-
	is_method(Mod,F,A),
	!,
	error(Mod, ignored_multifile_method(F, A)).
% 3rd case : valid multifile declaration.
class_treat_decl(multifile(F/A),[],Mod) :-
	atom(F),
	integer(A),
	!,
	asserta_fact(is_multifile(Mod,F,A)).
% 4th case : invalid multifile declaration.
class_treat_decl(multifile(S),[],Mod) :- !,
	error(Mod, invalid_multifile_decl(S)).
%% KEEP TRACKING OF DISCONTIGUOUS DECLARATIONS
class_treat_decl(discontiguous(F/A),[],Mod) :-
	is_attribute(Mod,F,A),
	!.
class_treat_decl(discontiguous(F/A),[],Mod) :-
	is_multifile(Mod,F,A),
	!.
class_treat_decl(discontiguous(F/A),[(:-discontiguous(NF/NA))],Mod) :-
	is_method(Mod,F,A),
	!,
	method_functor(F,A,NF,NA).
class_treat_decl(discontiguous(F/A),
	[(:- discontiguous(F/NA)),(:- discontiguous(NF/A))],_Mod) :-
	!,
	method_functor(F,A,NF,NA).
%% KEEP TRACKING OF CONCURRENT DECLARATIONS
class_treat_decl(concurrent(F/A),[],Mod) :-
	is_multifile(Mod,F,A),
	!.
class_treat_decl(concurrent(F/A),Exp,Mod) :- !,
	( is_concurrent(Mod,F,A) ->
	    true
	; asserta_fact(is_concurrent(Mod,F,A))
	),
	class_treat_decl(data(F/A),Exp,Mod).
%% PUBLIC PREDICATE DECLARATION
class_treat_decl(public(F/A),[],Mod) :-
	atom(F),
	integer(A),
	!,
	add_is_public(Mod,F,A).
class_treat_decl(public(S),[],Mod) :-
	!,
	error(Mod, invalid_public_decl(S)).
%% INHERITABLE PREDICATE DECLARATION
class_treat_decl(inheritable(F/A),[],Mod) :-
	atom(F),
	integer(A),
	!,
	add_is_inheritable(Mod,F,A).
class_treat_decl(inheritable(X),[],Mod) :- !,
	error(Mod, invalid_inheritable_decl(X)).
%% VIRTUAL PREDICATE DECLARATION
class_treat_decl(virtual(Module/A),[],Module) :-
	( F = Module ; F = destructor, A = 0 ),
	!,
	error(Module, virtual_not_allowed(F, A)).
class_treat_decl(virtual(F/A),[],Mod) :-
	atom(F),
	integer(A),
	!,
	( is_virtual(Mod,F,A) -> true ; asserta_fact(is_virtual(Mod,F,A)) ).
class_treat_decl(virtual(X),[],Mod) :- !,
	error(Mod, invalid_virtual_decl(X)).
class_treat_decl(_,_,_) :- !, fail.

has_addmodule([]) :- !, fail.
has_addmodule([addmodule|_]) :- !.
has_addmodule([_|N]) :- has_addmodule(N).

%%------------------------------------------------------------------------
%% Generate clauses at first-pass expansion

finish_class_trans(Module, FinalClauses) :- 
	interface_checking(Module),
	generate_fixed_clauses(Module,FixedClauses),
	generate_initial_state_clauses(Module,InitialStateClauses),
	generate_metapredicate_decls(Module,MetaPredDecls),
	generate_interface_info(Module,ItfDecls),
	generate_redefining_clauses(Module,RedefClauses),
	generate_reexport_clauses(Module,ReexportClauses),
	generate_implements_decls(Module,ImplementsDecls),
	%
	append(FixedClauses,InitialStateClauses,Aux1),
	append(Aux1,MetaPredDecls,Aux2),
	append(Aux2,ItfDecls,Aux3),
	append(Aux3,ReexportClauses,Aux4),
	append(Aux4,RedefClauses,Aux5),
	append(Aux5,ImplementsDecls,Aux6),
	append(Aux6,['$end$$of$$expansion$',end_of_file],FinalClauses).

%% Check interface information

interface_checking(Module) :-
	is_public(Module,F,A),
	check_public_info(Module,F,A),% Note: this goal may retract is_public/3
	fail.
interface_checking(Module) :-     % public preds are inheritable by default
	is_public(Module,F,A),
	add_is_inheritable(Module,F,A),
	fail.
interface_checking(Module) :-
	is_inheritable(Module,F,A),
	check_inheritable_info(Module,F,A),
	fail.
interface_checking(Module) :-
	is_virtual(Module,F,A),
	check_virtual_info(Module,F,A),
	fail.
interface_checking(_).

% 1st case: public predicate is a multifile: not allowed.
check_public_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_public(Module,F,A)),
	error(Module, public_multifile(F, A)).
% 2nd case: exported predicate is a destructor.
check_public_info(Module,destructor,0) :-
	error(Module, public_destructor),
	fail.
check_public_info(_,_,_).

% 1st case: inheritable predicate is a multifile: not allowed.
check_inheritable_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_inheritable(Module,F,A)),
	error(Module, inheritable_multifile(F, A)).
% 2nd case: inheritable predicate is a destructor.
check_inheritable_info(Module,destructor,0) :-
	error(Module, inheritable_destructor),
	fail.
% 3rd case: inheritable predicate was not defined in current source,
check_inheritable_info(Module,F,A) :-
	\+ is_method(Module,F,A),
	\+ is_attribute(Module,F,A),
	!,
	retract_fact(is_inheritable(Module,F,A)),
	error(Module, inheritable_ignored_not_defined(F, A)).
check_inheritable_info(_,_,_).

% 1st case: virtual predicate is a multifile: not allowed.
check_virtual_info(Module,F,A) :-
	is_multifile(Module,F,A),
	!,
	retract_fact(is_virtual(Module,F,A)),
	error(Module, virtual_multifile(F, A)).
% 2nd case: exported predicate was not defined in current source.
check_virtual_info(Module,F,A) :-
	\+ is_method(Module,F,A),
	\+ is_attribute(Module,F,A),
	!,
	retract_fact(is_virtual(Module,F,A)),
	error(Module, bad_virtual(F, A)).
check_virtual_info(_,_,_).

%% GENERATE FIXED END-OF-EXPANSION CLAUSES  (At first-pass expansion)

generate_fixed_clauses(Module,FixedClauses) :-
	FixedClauses =
	[
	    (:- multifile '$class$'/1),
	    (:- multifile 'class$super'/2),
	    (:- multifile 'class$initial_state'/3),
	    (:- multifile 'class$virtual'/4),
	    (:- multifile 'class$attr_template'/4),
	    (:- multifile 'class$default_cons'/1),
	    (:- multifile 'class$constructor'/2),
	    (:- multifile 'class$destructor'/3),
	    (:- multifile 'class$implements'/2),
	    (:- multifile '$ociao_accessible'/3),
	    (:- multifile 'goal$exp'/3),
	    (:- use_module(engine(internals), [last_module_exp/5])),
	    (:- use_module(engine(hiord_rt), ['$meta_call'/1])),
	    (:- redefining(mod_exp/5)),
	    ('$class$'(Module)),
            % Forces Ciao compiler to generate run-time info for this module.
	    ('$force$runtime$info$'(X) :- call(X))
	].

%% GENERATE STATE INITIALIZATION CLAUSES (At first-pass expansion)

generate_initial_state_clauses(Module,InitialStateClauses) :-
	findall('class$initial_state'(Module,F,Args),
	  initial_state(Module,F,Args),
	  InitialStateClauses).

%% GENERATE META PREDICATE SPECIFICATION DECLS. (at first-pass expansion)

generate_metapredicate_decls(Module,Decls) :-
	findall(
		   meta(F,A,Spec),
		   (is_metapredicate(Module,Spec),
		    functor(Spec,F,A)
		   ),
		   PreviousMetaDecls
	),
	metapredicate_decl(Module,PreviousMetaDecls,Decls).

metapredicate_decl(_,[],[]).
metapredicate_decl(Mod,[meta(F,A,Spec)|Nm],[(:- meta_predicate(NewSpec))|N]) :-
	is_method(Mod,F,A),
	!,
	method_head(Spec,'?',NewSpec),
	metapredicate_decl(Mod,Nm,N).
metapredicate_decl(Mod,[meta(F,A,_)|Nm],N) :-
	is_attribute(Mod,F,A),
	!,
	error(Mod, ignored_metapred_in_attribute(F, A)),
	metapredicate_decl(Mod,Nm,N).
metapredicate_decl(Mod,[meta(_,_,Spec)|Nm],[(:- meta_predicate(Spec))|N]) :-
	metapredicate_decl(Mod,Nm,N).

%% GENERATE CLASS INTERFACE INFO (at first-pass expansion)

generate_interface_info(Module,Interface) :-
	findall((:- virtual(F/A)),is_virtual(Module,F,A),VirtualList),
	findall((:- public(F/A)),is_public(Module,F,A),PublicList),
	findall((:- inheritable(F/A)),is_inheritable(Module,F,A),InhList),
	findall((:- attribute(F/A)),is_attribute(Module,F,A),AttrList),
	findall((:- method(F/A)),is_method(Module,F,A),MethodList),
	append(PublicList,InhList,Aux1),
	append(Aux1,AttrList,Aux2),
	append(Aux2,MethodList,Aux3),
	append(Aux3,VirtualList,Interface).

%% GENERATE REDEFINING/1 CLAUSES TO AVOID SOME ANNOYING WARNINGS
%% (at first-pass expansion)

generate_redefining_clauses(Module,Redef) :-
	findall(
		   (:- redefining(NewF/NewA)),
		   (
		         is_method(Module,F,A),
			 method_functor(F,A,NewF,NewA)
		   ),
		   Redef
	).

%% GENERATE REEXPORT CLAUSES
%% (at first-pass expansion)

generate_reexport_clauses(Module,Clauses) :-
	inherit_class(Module,SuperFile),
	!,
	Clauses = [(:- reexport(SuperFile))].
generate_reexport_clauses(_,[]).

%% GENERATE IMPLEMENTS/1 DECLARATIONS FOR .itf STORAGE
%% (at first-pass expansion)

generate_implements_decls(Module,Decl) :-
	findall(
		   (:- implements(Itf)),
		   implements(Module,Itf),
		   Decl
	).

%%------------------------------------------------------------------------
%%
%% METHOD BODY EXPANSION
%% (ALSO CALLED "SECOND PASS EXPANSION").
%%
%% method_expansion/4 is defined in class_tr_aux.pl
%%------------------------------------------------------------------------

class_clause_trans(clause(0,0),clause(0,0),Module) :-
	class_generate_oop_info(Module),
	additional_itf_checking(Module),
	generate_class_template(Module),
	fail.
class_clause_trans(clause(Head,_),_,_) :-
	class_ignore_clause(Head),
	!,
	fail.
class_clause_trans(clause(Head,Body),clause(Head,NewBody),Module) :-
	functor(Head,F,LastArgNumber),
	\+ is_multifile(Module,F,LastArgNumber),
	LastArgNumber > 0,
	arg(LastArgNumber,Head,LastArg),
	var(LastArg),
	!,
	body_expander(class_goal_expansion(LastArg),
	              class_fact_expansion(LastArg),
		      class_spec_expansion(LastArg),
		      Module,
		      Body,
		      NewBody).

class_ignore_clause('$end$$of$$expansion$').
class_ignore_clause('goal$exp'(_,_,_)).
class_ignore_clause(mod_exp(_,_,_,_,_)).

:- use_module(library(class(expansion_tools))).

%% EXPAND ONE GOAL

class_goal_expansion(Goal,InstanceID,Exp,Module):-
	class_goal_expansion_(Goal,Exp,Module,InstanceID).

%% unknown goal
class_goal_expansion_(Var,ID:Var,_,ID) :- var(Var), !.
%% Avoid invalid usage of method/attribute as module-qualified
class_goal_expansion_(A_Module:Goal,Exp,Module,ID) :-
	atom(A_Module),
	A_Module = Module,
	!,
	class_goal_expansion(Goal,ID,Exp,Module).
%% Goal is not related to this class
class_goal_expansion_(_:_,_,_,_) :-
	!,
	fail.
%% retrieving self instance ID 
class_goal_expansion_(self(Var),NewGoal,Module,InstVar) :- !,
	( var(Var) ->
	    NewGoal = class_rt:self(InstVar,Var)
	; error(Module, nonvar_self),
	  fail
	).
%% Goal belongs to assert/retract family
class_goal_expansion_(Goal,NewGoal,Module,InstVar) :-
	fact2attr(Goal,Fact,NewGoal), !,
	class_goal_expansion_fact(NewGoal,Fact,Goal,NewGoal,Module,InstVar).
%% Goal is a virtual method declared at this class,e.g.:
%% :- virtual v/1.
%% p(X) :- v(X).
class_goal_expansion_(Goal,(InstVar:Goal),Module,InstVar) :-
	functor(Goal,F,A),
	is_virtual(Module,F,A),
	!.
%% Goal is explicitly inherited
class_goal_expansion_(inherited(Goal),NewGoal,Module,InstVar) :- !,
	class_goal_expansion_inh(super,Goal,NewGoal,Module,InstVar).
class_goal_expansion_(Goal,NewGoal,Module,InstVar) :- !,
	class_goal_expansion_inh(self,Goal,NewGoal,Module,InstVar).

%% Goal is an attribute, e.g.: attr(I)
class_goal_expansion_inh(Ctx,Goal,class_rt:current_attr(InstFact,Inst),Module,Inst) :-
	functor(Goal,F,A),
	any_attribute_from(Ctx,Module,AtClass,F/A),
	!,
	Goal =.. [_|Args],
	attr_qualify(AtClass, F, InstF),
	InstFact =.. [InstF|Args].
%% Goal is a method, e.g.: mymethod(8,7)
class_goal_expansion_inh(Ctx,Goal,AtClass:NewGoal,Module,InstVar) :-
	functor(Goal,F,A),
	any_method_from(Ctx,Module,AtClass,F/A),
	!,
	method_head(Goal,InstVar,NewGoal).
%% Invalid goal
class_goal_expansion_inh(Ctx,Goal,_,Module,_) :-
	!,
	( Ctx = super ->
	    error(Module, unknown_inherited_goal(Goal)),
	    fail
	; Ctx = self ->
	    fail
	; fail
	).

%% Goal belongs to assert/retract family and involves 
%% virtual attribute.
class_goal_expansion_fact(NewGoal0,Fact,Goal,NewGoal,Module,InstVar) :-
	functor(Goal,AssrtPred,1),
	\+ functor(Fact,inherited,_),
	functor(Fact,F,A),
	is_virtual(Module,F,A),
	is_attribute(Module,F,A),
	!,
	functor(NewGoal,AssrtPred,1),
	arg(1,NewGoal,InstVar:Fact).
class_goal_expansion_fact(NewGoal,FactArg,Goal,NewGoal0,Module,InstVar) :-
	nonvar(FactArg),
	( FactArg = inherited(Fact) ->
	    class_goal_expansion_fact_ctx(super,NewGoal,Fact,Goal,NewGoal0,Module,InstVar)
	; Fact = FactArg,
	  class_goal_expansion_fact_ctx(self,NewGoal,Fact,Goal,NewGoal0,Module,InstVar)
	).

%% Goal belongs to assert/retract family
%% e.g.: asserta_fact(inherited attr(88)).
%% e.g.: retract(attr(_)).
class_goal_expansion_fact_ctx(Ctx,NewGoal,FactArg,Goal,NewGoal0,Module,InstVar) :-
	functor(Fact,F,A),
	( any_attribute_from(Ctx,Module,AtClass,F/A) ->
	    Fact =.. [_|Args],
	    attr_qualify(AtClass, F, NewF),
	    NewFact =.. [NewF|Args],
	    arg(1,NewGoal,NewFact),
	    arg(2,NewGoal,InstVar),
	    NewGoal0 = class_rt:NewGoal
	; ( Ctx = super ->
	      error(Module,unknown_inherited_attribute(Goal)),
	      NewGoal0 = fail
	  ; Ctx = self ->
	      fail
	  ; fail
	  )
	).

%% EXPAND ONE FACT

class_fact_expansion(Fact,Inst,Exp,Module) :-
	class_fact_expansion_(Fact,Exp,Module,Inst).

%% Fact is unknown
class_fact_expansion_(Fact,Inst:Fact,_,Inst) :-
	var(Fact),
	!.
%% Avoid invalid calls to class as a module.
class_fact_expansion_(A_Module:Fact,Exp,Module,Inst) :-
	atom(A_Module),
	A_Module = Module,
	!,
	class_fact_expansion(Fact,Inst,Exp,Module).
%% Fact is not related to current class.
class_fact_expansion_(_:_,_,_,_) :-
	!,
	fail.
%% Correct usage of explicitly inherited attribute.
class_fact_expansion_(inherited(Fact),Inst:Fact,Module,Inst) :-
	nonvar(Fact),
	functor(Fact,F,A),
	any_attribute_from(super,Module,_,F/A),
	!.
%% Incorrect usage of attribute
class_fact_expansion_(Fact,Fact,Module,_) :-
	functor(Fact,F,A),
	any_method_from(self,Module,_,F/A),
	!,
	error(Module, not_an_attribute(F, A)).
class_fact_expansion_(inherited(Fact),inherited(Fact),Module,_) :-
	functor(Fact,F,A),
	any_method_from(super,Module,_,F/A),
	!,
	error(Module, not_an_inherited_attribute(F, A)).
class_fact_expansion_(inherited(Fact),inherited(Fact),Module,_) :-
	!,
	error(Module, unknown_inherited_fact(Fact)).

%% EXPAND PREDICATE SPECIFICATION

class_spec_expansion(Spec,Inst,Exp,Module) :-
	class_spec_expansion_(Spec,Exp,Module,Inst).

%% spec is unknown
class_spec_expansion_(Spec,Inst:Spec,_,Inst) :-
	var(Spec),
	!.
class_spec_expansion_(F/A,Inst:F/A,_,Inst) :-
	(var(F) ; var(A)),
	!.
class_spec_expansion_(inherited(Spec),Inst:inherited(Spec),_,Inst) :-
	var(Spec),
	!.
class_spec_expansion_(inherited(F/A),Inst:inherited(F/A),_,Inst) :-
	(var(F) ; var(A)),
	!.
%% explicitly inherited attribute spec.
class_spec_expansion_(inherited(Spec),Spec2,Module,Inst) :- !,
	class_spec_expansion_inh(super,Spec,Spec2,Module,Inst).
class_spec_expansion_(Spec,Spec2,Module,Inst) :-
	class_spec_expansion_inh(self,Spec,Spec2,Module,Inst).

class_spec_expansion_inh(Ctx,F/A,NewSpec,Module,Inst) :-
	atom(F),
	integer(A),
	A >= 0,
	( any_attribute_from(Ctx,Module,_,F/A) ->
	    NewSpec = Inst:F/A
	; any_method_from(Ctx,Module,AtClass,F/A) ->
	    method_functor(F, A, NewF, NewA),
	    '$module_concat'(NewF, AtClass, MNewF),
	    NewSpec = F/A
	; ( Ctx = super ->
	      NewSpec = inherited(F/A),
	      error(Module, unknown_inherited_spec(F/A))
	  ; Ctx = self ->
	      fail
	  ; fail
	  )
	).

%% MAP ASSERT/RETRACT PREDICATES TO ASSERT_ATTR/RETRACT_ATTR PREDICATES

fact2attr(asserta(Fact),        Fact, asserta_attr(_,_)    ).
fact2attr(assertz(Fact),        Fact, assertz_attr(_,_)    ).
fact2attr(assert(Fact),         Fact, assert_attr(_,_)     ).
fact2attr(retract(Fact),        Fact, retract_attr(_,_)    ).
fact2attr(retractall(Fact),     Fact, retractall_attr(_,_) ).
fact2attr(abolish(Fact),        Fact, retractall_attr(_,_) ).
fact2attr(assertz_fact(Fact),   Fact, assertz_attr(_,_)    ).
fact2attr(asserta_fact(Fact),   Fact, asserta_attr(_,_)    ).
fact2attr(retract_fact(Fact),   Fact, retract_attr(_,_)    ).
fact2attr(retract_fact_nb(Fact),Fact, retract_attr_nb(_,_) ).
fact2attr(retractall_fact(Fact),Fact, retractall_attr(_,_) ).
fact2attr(erase(Fact),          Fact, retractall_attr(_,_) ).
fact2attr(current_fact(Fact),   Fact, current_attr(_,_)    ).
fact2attr(current_fact_nb(Fact),Fact, current_attr_nb(_,_) ).
fact2attr(set_fact(Fact),       Fact, set_attr(_,_)        ).

% Generate class metadata (at second-pass)

:- use_module(engine(rt_exp), ['$module_concat'/3]).

% Import all methods which may be called from this class.
generate_class_template(Module) :-
	% TODO: are those different? why?
	( implementation(Module,method,AtClass,F,A)
	; inherited_pred(Module,method,AtClass,F,A)
	),
	method_functor(F,A,NewF,NewA),
	add_imports(Module,AtClass,NewF,NewA,AtClass),
	fail.
% Generate attribute template for new/2 to create objects.
generate_class_template(Module) :-
	attribute_set(Module,AtClass,F,A),
	attr_qualify(AtClass, F, QfdRealF),
	attribute_kind(Module,F,A,Kind),
	add_clause(Module,'class$attr_template'(Module,QfdRealF,A,Kind)),
	fail.
% Generate implemented interface info
% Used by runtime type tests.
generate_class_template(Module) :-
	impl_interface(Module,Itf),
	add_clause(Module,'class$implements'(Module,Itf)),
	fail.
% Generate inheritance relationship info.
% Used by runtime type tests.
generate_class_template(Module) :-
	super(Module,Super),
	add_clause(Module,'class$super'(Module,Super)),
	fail.
% Generate constructor info
% Used by new/2.
generate_class_template(Module) :-
	is_method(Module,Module,Arity),
	functor(Cons,Module,Arity),
  	add_clause(Module,'class$constructor'(Cons,Module)),
	fail.
generate_class_template(Module) :-
	\+ is_method(Module,Module,_),
	add_clause(Module,'class$default_cons'(Module)),
	fail.
% Generate destructor info
% Used by destroy/1.
generate_class_template(Module) :-
	is_method(Module,destructor,0),
	method_head(destructor,Inst,CodeDestr),
	'$module_concat'(CodeDestr,Module,MetaCall),
	add_clause(Module,'class$destructor'(Module,Inst,MetaCall)),
	fail.
% Generate runtime expansor needed when meta-programming is 
% present at user code. Those clauses will be called from class_rt:mod_exp/5.
% (But those are not multifile ir order to preserve Prolog indexing...)
% Notice that this expansor is not related to Var:goal. It is used
% for goals such as X=inherited(goal(1)),call(X).
generate_class_template(Module) :-
	( Ctx = self, Goal2 = Goal ; Ctx = super, Goal2 = inherited(Goal) ),
	( any_attribute_from(Ctx, Module,AtClass,F/A), Kind = attribute
	; any_method_from(Ctx, Module,AtClass,F/A), Kind = method
	),
	( decl_virtual(Module, F/A) -> What = virtual_mod_exp
	; Kind = attribute -> What = functor_concat(AtClass)
	; Kind = method -> What = lastmodexp(AtClass)
	),
	functor(Goal,F,A),
	add_clause(Module,'goal$exp'(Goal2,Module,What)),
	fail.
% Generate interface information (accesibility information).
% Used by runtime expansors and relatives...
generate_class_template(Module) :-
	implementation(Module,K,AtClass,F,A),
	( public_pred(Module,K,F,A) ->
            % Predicate is world-accessible
	    accessible_check(F,A,Module,_,Access),
	    add_clause(Module,Access)
	; % Predicate is accessible by this class
          accessible_check(F,A,Module,Module,Access),
	  add_clause(Module,Access),
	  ( AtClass \== Module ->
	      % Predicate is accessible by inh. class
	      accessible_check(F,A,Module,AtClass,AccessAux),
	      add_clause(Module,AccessAux)
	  ; true
	  )
	),
	fail.
generate_class_template(Module) :-
	add_clause2(Module,
	  mod_exp(Type, MethodHead, From, Inst, Exp),
	  class_rt:gen_mod_exp(Type, MethodHead, From, Module, Inst, Exp)),
	fail.
% Generate run-time expansor for virtual calling
% stored at multifile sheet...
generate_class_template(Module) :-
	virtual_pred_template(Module,Module),
	fail.
generate_class_template(_).

accessible_check(F,A,Module,From,'$ociao_accessible'(Goal,Module,From)) :-
	functor(Goal,F,A).

attribute_kind(Module,F,A,concurrent) :-
	is_concurrent(Module,F,A),
	!.
attribute_kind(_,_,_,dynamic).

virtual_pred_template(Module,FromClass) :-
	% Visible methods in FromClass
	( Kind = method,
	  ( decl_method(FromClass, F/A), From = FromClass
	  ; inherited_pred(FromClass, method,From,F,A)
	  )
	; Kind = attribute,
	  ( decl_attribute(FromClass, F/A), From= FromClass
	  ; inherited_pred(FromClass, attribute,From,F,A)
	  )
	),
	decl_virtual(FromClass, F/A),
	% Obtain AtClass, most recent than From, from Module
	( ( Kind = method,
	  ( decl_method(Module, F/A), AtClass = Module
	  ; inherited_pred(Module, method,AtClass,F,A)
	  )
	; Kind = attribute,
	  ( decl_attribute(Module, F/A), AtClass = Module
	  ; inherited_pred(Module, attribute,AtClass,F,A)
	  )
	) -> true ; fail ), % force cut
	functor(Goal,F,A),
	( Kind = attribute -> What = functor_concat(AtClass)
	; Kind = method -> What = lastmodexp(AtClass)
	),
	% TODO: meaning: The method Goal of FromClass is redefined in Module
	% when Goal is called from FromModule, the class is extracted from the object ID and a 'class$virtual'(Class, FromModule, Goal, ...) is called to obtain the real implementation... SOOOOO, what we really want is to know that Module is a descendant of FromClass... then use normal module expansion (the problem: visibility rules will make name resolution more complicated ... here is where the constraints can optimize things: P:a...)
	add_clause(Module,'class$virtual'(Module,FromClass,Goal,What)),
	fail.
virtual_pred_template(Module, FromClass) :-
	decl_super(FromClass, Super),
	!,
	virtual_pred_template(Module, Super).
virtual_pred_template(_,_).

%% PERFORM ADDITIONAL ITF CHECKING

:- include(.(class_common)).
	
additional_itf_checking(Module) :-
	interface_itf_checking(Module),
	fail.
% Super class was not class-expanded
additional_itf_checking(Module) :-
	super(Module,Super),
        \+ defines_a_class(Super),
	error(Module, inherited_nonclass(Super)),
	retract_fact(super(Module,_)),
	fail.
% Circular inheritance is present
additional_itf_checking(Module) :-
	super(Module,Super),
	get_inheritance_line(Super,InhLine),
	member(Module,InhLine),
	error(Module, circular_inheritance(Super)),
	retract_fact(super(Module,_)),
	fail.
% There is no call to inherited constructor
additional_itf_checking(Module) :-
	super(Module,Super),
	inherited_pred(Module,method,Super,Super,_),
	\+ is_method(Module,Module,_),
	error(Module, inherited_constructor_not_called),
	fail.
% multifile F/A is also an inherited method or attribute definition
additional_itf_checking(Module) :-
	is_multifile(Module,F,A),
	inherited_pred(Module,_,_,F,A),
	error(Module, multifile_hides_inherited(F, A)),
	fail.
% visibility of redefined F/A is more restrictive than the inherited one
additional_itf_checking(Module) :-
	( decl_method(Module, F/A),
	; decl_attribute(Module, F/A)
	),
	inherited_pred(Module,_,_,F,A),
	\+ decl_inheritable(Module, F/A),
	\+ decl_public(Module, F/A),
	error(Module, hides_inheritable(F, A)),
	fail.
additional_itf_checking(Module) :-
	( decl_method(Module, F/A),
	; decl_attribute(Module, F/A)
	),
	inherited_pred(Module,_,_,F,A),
	public_pred(Module,_,F,A),
	\+ decl_public(Module, F/A),
	error(Module, hides_public(F, A)),
	fail.
% public F/A is unknown
additional_itf_checking(Module) :-
	decl_public(Module, F/A),
	\+ decl_method(Module, F/A),
	\+ decl_attribute(Module, F/A),
	\+ inherited_pred(Module,_,_,F,A),
	error(Module, not_defined_or_inherited(F, A)),
	fail.
additional_itf_checking(_).

get_inheritance_line(Class,[Class|N]) :-
	decl_super(Class, Super),
	!,
	get_inheritance_line(Super,N).
get_inheritance_line(Class,[Class]).

%%------------------------------------------------------------------------
%%
%% OTHER AUXILIARY PREDICATES
%%
%%------------------------------------------------------------------------

%% Convert sequence to list
:- export(sequence_to_list/2).
sequence_to_list(Sequence,List) :-
	functor(Sequence,',',2),
	!,
	arg(1,Sequence,Sq1),
	arg(2,Sequence,Sq2),
	sequence_to_list(Sq1,L1),
	sequence_to_list(Sq2,L2),
	append(L1,L2,List).
sequence_to_list(SomeThing,[SomeThing]).

:- use_module(library(terms), [atom_concat/2]).

%% Convert a method head to its implementation head
:- export(method_head/3).
method_head(Method,Obj,Code) :-
	var(Code),
	Method =.. [F|Args],
	append(Args,[Obj],NewArgs),
	atom_concat('obj$',F,NewF),
	Code =.. [NewF|NewArgs].

:- export(method_functor/4).
method_functor(F, A, NF, NA) :-
	atom_concat('obj$', F, NF),
	NA is A+1.

:- export(get_method_id/2).
get_method_id(Head, Id) :-
	functor(Head,F,A),
	atom_concat('obj$', _, F),
	arg(A,Head,Id).

:- export(attr_qualify/3).
attr_qualify(Class, F, MF) :-
	atom_concat([':',Class,'::',F],MF).

%%------------------------------------------------------------------------
%% Add new clauses on second-pass expansion
%%------------------------------------------------------------------------

add_clause(Module,Head) :-
	add_clause_of(Module, Head, true).

%%------------------------------------------------------------------------

% General access to methods or attributes from self or super class

any_method_from(self, Module,From,F/A) :- !,
	( decl_method(Module, F/A) -> From = Module
	; inherited_pred(Module,method,From,F,A)
	).
any_method_from(super, Module,From,F/A) :-
	inherited_pred(Module,method,From,F,A).

any_attribute_from(self, Module,From,F/A) :- !,
	( decl_attribute(Module, Spec) -> From = Module
	; inherited_pred(Module,attribute,From,F,A)
	).
any_attribute_from(super, Module,From,F/A) :-
	inherited_pred(Module,attribute,From,F,A).
