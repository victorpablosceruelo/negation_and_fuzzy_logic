:- module(api_direc_assrt,
	    [cleanup_assrt/0,
	     % assertions
	     get_all_assertions/2, get_assertion/3, get_assertion/2,
	     get_assertions/2, get_assertions/4, get_all_assertions_key/1,
	     get_all_assertions_key_of_module/2, get_commented_assertions/2,
	     add_assertions/1, add_assertion/1, add_commented_assertions/1,
	     add_commented_assertion/1, erase_assertions/1, erase_assertion/1,
	     update_assertion/2,
	     % dictionaries
	     analysis_info_to_assertions/0, has_assertions/1,
	     has_assertions/3,
	     % directives
	     is_directive/1, get_directives/1, get_directives/2,
	     get_directive/1, add_directive/1, erase_directive/1],
	     [condcomp, assertions, regtypes, ciaopp_options]).

:- use_module(library(aggregates)).
:- use_module(library(messages)).
:- use_module(program(itf_db),    [curr_file/2]).
:- use_module(program(clause_db), [source_clause/3, clause_locator/2]).
:- use_module(program(assrt_db),
	    [
		assertion_read/9,
		assertion_body/7,
		add_assertion_read/9,
		remove_assertion_read/9,
		ref_assertion_read/10
	    ]).

:- use_module(ciaopp(api(api_internal_types))).

:- use_package(.(api_internal_dec)).

:- use_module(ciaopp(api(api_base))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLEAN UP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cleanup_assrt :-
	retractall_fact(commented_as(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ASSERTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ADD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate get_assertions(?, ?, goal, ?).

get_all_assertions_key(L2) :-
% 	findall( ClKey , 
% 	       assertion_read( ClKey , _ , _Status , _Type , _Body , _ , 
% 	                       _ , _ , _ ),
% 		L ),
% 	remove_repited( L , [] , L2 ),
% 	!.
	setof(ClKey,
	    M^Status^Type^Body^Dic^S^LE^LB^
	    assertion_read(ClKey, M, Status, Type, Body, Dic,
		S, LE, LB),
	    L2),
	!.

get_all_assertions_key_of_module(M, L2) :-
	findall(ClKey,
	    assertion_read(ClKey, M, _, _, _, _, _, _, _),
	    L),
	remove_repited(L, [], L2),
	!.
get_all_assertions_key_of_module(_, []).

get_assertions(ClKey, Ass, Cond, AssList) :-
	findall(Ass, get_assertion(ClKey, Ass, Cond), AssList).
get_assertions(ClKey, AssList) :-
	findall(Ass, get_assertion(ClKey, Ass), AssList).

get_all_assertions(ClKey, AssList) :-
	findall(Ass, get_assertion(ClKey, Ass), AssList).

:- meta_predicate get_assertion(?, ?, goal).

get_assertion(ClKey, Ass, Condition) :-
	nonvar(Condition),
	get_assertion(ClKey, Ass),
	\+ \+ Condition.

get_assertion(ClKey, Ass) :-
	Ass = as${ref => ID,
	    status => Status,
	    type => Type,
	    head => Key,
	    compat => Compat,
	    call => Call,
	    succ => Succ,
	    orig_call => Call,
	    orig_succ => Succ,
	    comp => Comp,
	    dic => Dic,
	    comment => Comment,
	    locator => Loc
	},
	Loc = loc${file => S,
	    line_begin => LB,
	    line_end => LE,
	    module => M
	},
	ref_assertion_read(ClKey, M, Status, Type, Body, Dic,
	    S, LB, LE, ID),
	assertion_body(Key, Compat, Call, Succ, Comp, Comment, Body).

get_commented_assertions(ClKey, Ass) :-
	findall(A1,
	    ( current_fact(commented_as(A1)),
		A1 = as${head => ClKey} ),
	    Ass).

%%% ADD

:- set_prolog_flag(multi_arity_warnings, on).

:- pred add_assertions(AssrtList) : list(AssrtList, t_as)
# "Add assertions list @var{AssrtList} to internal DB.".

add_assertions([]).
add_assertions([A|As]) :-
	add_assertion(A),
	add_assertions(As),
	!.

:- pred add_assertion(Assrt) : t_as(Assrt)
# "Add assertion @var{Assrt} to internal DB.".

add_assertion(As) :-
	As = as${
	    ref => ID,
	    status => Status,
	    type => Type,
	    head => Head,
	    compat => Compat,
	    call => Calls,
	    succ => Succ,
	    comp => Comp,
	    dic => Dic,
	    locator => AsLoc,
	    comment => Com
	},
	add_assertion_rt(As),
	AsLoc = loc${file => S,
	    line_begin => LB,
	    line_end => LE,
	    module => M},
	assertion_body(Head, Compat, Calls, Succ, Comp, Com, Body),
	add_assertion_read(Head, M, Status, Type, Body, Dic, S, LB, LE),
	ref_assertion_read(Head, M, Status, Type, Body, Dic, S, LB, LE, ID),
	set_modified_flag,
	!.
add_assertion(As) :-
	error_message("Internal Error: add_assertion: Could not add ~p", [As]).

:- if(defined(api_rt)).
add_assertion_rt(As) :-
	As = as${
	    ref => ID,
	    status => Status,
	    type => Type,
	    head => Head,
	    compat => Compat,
	    call => Calls,
	    succ => Succ,
	    comp => Comp,
	    dic => Dic,
	    locator => AsLoc,
	    comment => Com,
	    fromwhere => From
	},
	add_as_check(var(ID),        "id ~q is not var",        [ID]),
	add_as_check(ground(Status), "status ~q is not ground", [Status]),
	add_as_check(ground(Type),   "type ~q is not ground",   [Type]),
	add_as_check((nonvar(Calls), list(Calls)),
	    "calls ~q has to be a list",   [Calls]),
	add_as_check((nonvar(Compat), list(Compat)),
	    "compat ~q has to be a list",  [Compat]),
	add_as_check((nonvar(Succ), list(Succ)),
	    "success ~q has to be a list", [Succ]),
	add_as_check(nonvar(Comp), "comp ~q is var",           [Comp]),
	add_as_check(nonvar(Dic),  "dic ~q is var",            [Dic]),
	add_as_check(ground(Com),  "comment ~q is not ground", [Com]),
	add_as_check(( ground(AsLoc),
		AsLoc = loc${file => S,
		    line_begin => LB,
		    line_end => LE,
		    module => M
		}
	    ),
	    "locator ~q has to be of type loc${...}", [AsLoc]),
	(ground(From), ! ; From = asserted).

:- meta_predicate add_as_check(goal, ?, ?).
add_as_check(Goal, Message, Opts) :-
	api_check(Goal, Message, Opts, add_assertion).

:- else.
add_assertion_rt(_).
:- endif.

:- pred add_commented_assertions(A)
	: list(A, t_as)
# "Add the assertions list @var{A} to the commented assertions DB.".

:- data commented_as/1.

add_commented_assertions([]).
add_commented_assertions([A|As]) :-
	add_commented_assertion(A),
	add_commented_assertions(As).



:- pred add_commented_assertion(A)
	: term(A)
# "Add assertion @var{A} to the commented assertions DB.".

add_commented_assertion(A) :-
	A = as${status => S, type => T, head => H, compat => C,
	    call => Ca, succ => Su, comp => Co, dic => D,
	    orig_call => Ca, orig_succ => Su,
	    locator => L, comment => Com},
	!,
	A1 = as${status => S, type => T, head => H, compat => C,
	    call => Ca, succ => Su, comp => Co, dic => D,
	    orig_call => Ca, orig_succ => Su,
	    locator => L, comment => Com, fromwhere => commented},
	assertz_fact(commented_as(A1)).
add_commented_assertion(A) :-
	error_message("INTERNAL ERROR: add_commented_assertion: "||
	    "~q is not a valid assertion", [A]).



%%%   ERASE

erase_assertions([]) :-
	!.
erase_assertions([A|As]) :-
	erase_assertion(A),
	!,
	erase_assertions(As).

% erase_assertions( [ A | As ] ) :-
% 	!,
% 	message( error , ['erase_assertions: Invalid assertion: ' , A] ),
% 	erase_assertions( As ).

erase_assertion(A) :-
	A = as${ref => Ref},
	nonvar(Ref),
	erase(Ref),
	!.
erase_assertion(A) :-
	A = as${
	    status => Status,
	    type => Type,
	    head => Head,
	    compat => Compat,
	    call => Call,
	    succ => Succ,
	    comp => Comp},
	assertion_body(Head, Compat, Call, Succ, Comp, [], Body),
	remove_assertion_read(Head, _, Status, Type, Body, _, _, _, _),
	set_modified_flag.
erase_assertion(A) :-
	error_message("The assertion: ~p has invalid REF", [A]).


%%%   UPDATE

:- pred update_assertion(OldAs, NewAs)
# "Updates the assertion @var{OldAs} with @var{NewAs}.".

% --- a version with ref nonvar should exists?
update_assertion(OldAs, NewAs) :-
	erase_assertion(OldAs),
	!,
	add_assertion(NewAs).

update_assertion(OldAs, _NewAs) :-
	error_message("Internal Error: update_assertions: ref field "||
	    "is neccesary to update. Old Assertion: ~p~n", [OldAs]),
	fail.




:- if(defined(mini_pp)).

% --- DTM: To define hooks here
analysis_info_to_assertions.

:- else.

% --- DTM: This has to be part of the API SOON!
:- use_module(infer(prepare_ai_output)).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(infer(infer_db),          [domain/1]).
:- use_module(infer(infer_dom),         [non_collapsable/1]).


analysis_info_to_assertions :-
	curr_file(_, M),
% Delete true assertions
	cleanup_output(M),
	current_pp_flag(collapse_ai_vers, Collapse0),
	current_pp_flag(pp_info,          PPoints),
	current_fact(domain(AbsInt)),
	(non_collapsable(AbsInt) -> Collapse=off ; Collapse=Collapse0),
% We want to remove choose points in prepare_ai_output
	(prepare_ai_output(PPoints, cl, AbsInt, Collapse) -> true),
	fail.
analysis_info_to_assertions.

:- endif.


:- push_prolog_flag(multi_arity_warnings, on).

:- pred has_assertions(ClKey, Ass, Cond)

# "Success if the clause key @var{ClKey} has assertions @var{Ass} that
  hold the condition @var{Cond}.".

:- meta_predicate has_assertions(?, ?, goal).

has_assertions(ClKey, Ass, Cond) :-
	get_assertion(ClKey, Ass),
	\+ \+ Cond.


:- pred has_assertions(ClKey)

# "Success if the clause key @var{ClKey} has assertions.".

has_assertions(ClKey) :-
	get_assertion(ClKey, _).

:- pop_prolog_flag(multi_arity_warnings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   DIRECTIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% IS

% --- deprecated
is_directive(((directive(_) :_), _)).


%%% GET

:- pred get_directive(D)
	: (t_direc(D) ; var(Direcs))
	=> t_direc(D)
# "Complete directive info of @var{D}. If @var{D} is a variable, then
  it gets all directives by fail.".

get_directive(Direc) :-
	Direc = direc${ref => Ref,
	    key => Key,
	    body => Body,
	    dic => Dict,
	    locator => Loc},
	current_fact(source_clause(Key, directive(Body), Dict), Ref),
	\+ special_directive(Body),
	clause_locator(Key, Loc).

special_directive(D) :-
	functor(D, F, _),
	member(F, [comp, exit, entry, true, false,
	  success, call, check, checked, pred, % Assertion 
	  module]).



:- pred get_directives(Direcs)
	: var(Direcs)
	=> list(Direcs, t_direc)
# "Retrives all directives.".

get_directives(Direcs) :-
	findall(A, get_printable_directive(A), Direcs).

get_printable_directive(Direc) :-
	curr_file(Src, _),
% 	Only print directives declared in the current file:
	Direc = direc${locator => loc(Src, _, _)},
	get_directive(Direc).

:- pred get_directives(D, Direcs)
	: (t_direc(D), var(Direcs))
	=> list(Direcs, t_direc)
# "Retrives all directives in @var{Direcs} that match with @var{D}".

get_directives(D, Direcs) :-
	findall(D, get_directive(D), Direcs).



%%%   ADD


:- pred add_directive(C) : t_direc(C)
# "The directive @var{C} is added to the program data base. This
  directive will be considered as read from the program, i.e.,
  analizers, transformations and output will use/show it.".

:- pred add_directive(C) : term(C)
# "This call is used to complete the @pred{t_direc/1} fields when
  programmer writes something like add_directive( use_module(lists) )
  or add_directive( redefining( append/3 ) ).".

add_directive(Direc) :-
	Direc = direc${key => Key, body => Body, dic => Dict, locator =>
	    Unknown, ref => R},
	!,
% Define unkown locator if not passed
	(loc_unknown(Unknown) -> true ; true),
	( current_fact(source_clause(Key, directive(Body), Dict))
	-> true
	; asserta_fact(source_clause(Key, directive(Body), Dict), R1),
	    set_modified_flag,
	    ( var(R)
	    -> R = R1
	    ; error_message("add_directive: Asserting directive " ||
		    "with non-var reference.") )
	).

% call like add_directive( use_module( whatever ) ) or add_directive(
% redefining( append/3 ) ).
add_directive(Body) :-
	Direc = direc${key => Key, body => Body, dic => Dict, locator =>
	    Unknown},
	get_key(Body, Key),
	null_dict(Dict),
	loc_unknown(Unknown),
	add_directive(Direc).



:- pred erase_directive(D)
	: (t_direc(D) ; term(D))

# "Erase directive @var{D} from data-base".

% Normal case: we have the reference
erase_directive(Direc) :-
	Direc = direc${ref => R},
	nonvar(R),
	!,
	set_modified_flag,
	erase(R).
erase_directive(Direc) :-
	Direc = direc${key => Key, body => Body, dic => Dict
	},
	!,
	set_modified_flag,
	retractall_fact(source_clause(Key, directive(Body), Dict)).
erase_directive(Body) :-
	get_key(Body, Key),
	Direc = direc${key => Key, body => Body
	},
	erase_directive(Direc).
