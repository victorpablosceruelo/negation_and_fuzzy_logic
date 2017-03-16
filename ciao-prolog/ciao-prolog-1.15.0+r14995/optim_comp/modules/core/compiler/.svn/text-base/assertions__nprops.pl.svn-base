% (included from module_state.pl)
%:- class(assertions__nprops, [], [compiler(complang)]).

% Other libraries
:- use_module(library(assertions(assertions_props))).
%%:- use_module(library(assertions(c_itf_props))).
:- use_module(compiler(assertions__common)).
:- use_module(compiler(assertions__syntactic)).
:- use_module(library(lists), [append/3]).

:- use_module(compiler(errlog)).

% TODO: fix documentation
% :- pred normalize_assertions_pass_one/1 # "For each assertion
%    normalizes the assertion body (but not the properties inside or
%    those properties and modes which appear in the head). The predicate
%    descriptor (both that in the assertion body and the first argument
%    of @pred{assertion_read/9}) is partially normalized in that
%    @tt{functor/arity} formats are expanded into terms, but modes and
%    properties in arguments are left for the second pass (i.e.,
%    @tt{p(X,+)} may be present, but not @tt{p/2}). However, if the
%    assertion is a @tt{modedef} then it is fully normalized at this
%    time (including body properties, which are normalized but not
%    checked) so that @pred{normalize_assertions_pass_two/1} can use it
%    later while normalizing other assertions. The (partially)
%    normalized assertion is left asserted as an @pred{assertion_read/9}
%    fact.".
% 	
% :- pred normalize_assertions_pass_two_opts/3 # "For each assertion
%    left by @pred{normalize_assertions_pass_one/1} (except for
%    @tt{modedef} mode declarations, which are left as is) extracts all
%    head properties and modes, normalizes all body properties, adds the
%    head properties and the properties implied by the head modes to the
%    body properties, and checks that a definition (or, at least, a
%    declaration) is available for each property (issuing a warning
%    otherwise). The old (partially) normalized assertion is eliminated
%    and the fully normalized assertion is left asserted (again as an
%    @pred{assertion_read/9} fact) in its place. Body property
%    conjunctions are (currently) represented as lists to facilitate
%    processing. It admits options in the second argument.".

{
:- extends modread_ctx.
%:- public do_modedef/1.
% TODO: Add '::' as a mode? E.g. :- pred append(X::list, Y::list, Z::list).
% TODO: Add 'meta' as a special mode? (for meta_predicates)
%       Pro: compact syntax; Cons: possibly conflicting assertions
do_modedef(AssrtBody0) :-
	errs :: errlog <- ~get_errs,
	normalize_properties(AssrtBody0,AssrtBody,[],modedef), % special case for modedef
	assertion_body(Head,_,_,_,_,_,AssrtBody),
	(~def_envmod).add_modedef(Head, AssrtBody).
}.

{
:- extends modread_ctx.
%:- public normalize_assertion/2.
% Normalize one assertion
normalize_assertion(Assertion0, Assertion) :-
	errs :: errlog <- ~get_errs,
	Assertion0 = a(AStatus,AType,Assrt0,Loc),
	errs.add_loc(Loc),
	Opts = [], % TODO: it was ['-modes'] or [] -- recover?
	Ok = ( normalize_properties(Assrt0,Assrt,Opts,AType) ? yes | no ),
	errs.del_loc,
	Ok = yes,
	Assertion = a(AStatus,AType,Assrt,Loc).
}.

%% ---------------------------------------------------------------------------

{
:- extends modread_ctx.

:- pred normalize_properties(Ass,NAss,Opts,AType) 

   : nabody(Ass) => nabody(NAss) 

   # "The body of @var{NAss} contains the normalized versions of the
     properties in the head and body of @var{Ass}. Body @em{structure}
     is assumed to be normalized in @var{Ass}(i.e., it is assumed that
     the assertion has already been filtered by
     @pred{normalize_body/2}).".

normalize_properties(Ass,NAss,Opts,AType) :-
	errs :: errlog <- ~get_errs,
       	assertion_body(PD,DP,CP,AP,GP,CO,Ass),
	% Normalize properties and modes in head
        ( AType = modedef ->
	    ADP = [],
	    ACP = [],
	    AAP = [],
	    AGP = [],
	    NPD = PD
	; get_head_arg_props(PD,NPD,ADP,ACP,AAP,AGP,Opts,AType)
	),
	% Normalize properties written in "prop * prop" format, 
        % turn conjuction into a list (not such a good idea?)
	functor(PD,_,Arity),
	norm_arg_props(DP,NDP,NPD,Arity),
	norm_arg_props(CP,NCP,NPD,Arity),
	norm_arg_props(AP,NAP,NPD,Arity),
	% Normalize properties written as "prop" (rather than "prop(Goal)")
	norm_goal_props(GP,NGP,NPD),
	% Add head arg props to the other props found
	append(ADP,NDP,CNDP),
	append(ACP,NCP,CNCP),
	append(AAP,NAP,CNAP),
	append(AGP,NGP,CNGP),
       	assertion_body(NPD,CNDP,CNCP,CNAP,CNGP,CO,NAss),
	!.
normalize_properties(Ass,_,_,AType) :-
	errs :: errlog <- ~get_errs,
       	assertion_body(PD,  _DP, _CP, _AP, _GP,_CO,Ass),
	% TODO: use exceptions?
	errs.compiler_error(assertion_syntax_for(AType, PD)),
	fail.
}.
	
%% ---------------------------------------------------------------------------
{
:- extends modread_ctx.

:- pred get_head_arg_props(Head,NPD,NDP,NCP,NAP,GP,Opts,AType) 
   => (list(NDP),list(NCP),list(NAP),list(GP))

   # "@var{Head} is a head descriptor whose arguments possibly include
      mode annotations. These get translated into standard
      properties. @var{NPD} is the new head descriptor. @var{NDP}
      contais the new compatible properties. @var{NCP}
      contais the new call properties. @var{NAP} contains the new
      answer properties.".

get_head_arg_props(PD,NPD,DP,CP,AP,GP,Opts,AType) :-
	functor(PD, F,A),
	functor(NPD,F,A),
	transform_head_arg_props(0,A,PD,NPD,DP,CP,AP,GP,F,A,Opts,AType).

transform_head_arg_props(Last,Last,_PD,_NPD,[],[],[],[],_,_,_,_) :-
	!.
transform_head_arg_props(PArg,Last,PD,NPD,DP,CP,AP,GP,F,A,Opts,AType) :-
	Arg is PArg+1,
	arg(Arg,PD,PDA),
	arg(Arg,NPD,NPDA),
	get_arg_props(PDA,NPDA,DP-DPT,CP-CPT,AP-APT,GP-GPT,NPD,F,A,Opts,AType),
	transform_head_arg_props(Arg,Last,PD,NPD,DPT,CPT,APT,GPT,F,A,Opts,AType).

%% Handling of ISO standard-like "modes" and properties which appear 
%% literally in the head.
%% 
%% p(+A) p(+) p(int) p(+int) p(+list(int)) ...
%% p(ilist(A,integer)) (parametric mode)
%% 
%% Argument is a variable - do nothing
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_Opts,_AType) :-
	var(PDA),
	!.
%% Argument is a defined (possibly parametric) mode, 
get_arg_props(PDA,NPDA,NDP,NCP,NAP,NGP,NPD,_F,_A,Opts,AType) :- 
	with_or_without_arg(PDA,NNPDA,Prop),
	( % TODO: wrong, use enclosing_star (resolution)
	  (~top_envmod).get_modedef(Prop, NPropAss) -> 
	    true
	; fail
	), 
	(  member('-modes',Opts), \+ propfunctor(AType)
	-> %% Keep modes (and their properties!): do nothing.
	   NPDA = PDA, NDP=DL-DL, NCP=CL-CL, NAP=AL-AL, NGP=GL-GL
	;  %% Assumed that the Props have already been put in list form!
	   NPDA = NNPDA,
	   assertion_body(_Prop, CompatProps, CallProps, AnswerProps, GoalProps, _, NPropAss),
	   !,
	   resolve_applications(CompatProps,ACompatProps),
	   diff_append_props(ACompatProps,NDP),
	   resolve_applications(CallProps,ACallProps),
	   diff_append_props(ACallProps,NCP),
	   resolve_applications(AnswerProps,AAnswerProps),
	   diff_append_props(AAnswerProps,NAP),
           % Goal Props in modedef should have to be normalized at this point.
           % Since now they come normalized as normal properties, first 
	   % denormalize a bit (to conj) and then fully normalize:
%% 	   list_to_conj(GoalProps,CGoalProps),
%% 	   norm_goal_props(CGoalProps,NGoalProps,NPD),
	   norm_goal_props(DNGoalProps,GoalProps,_),
	   norm_goal_props(DNGoalProps,NGoalProps,NPD),
	   resolve_applications(NGoalProps,AGoalProps),
	   diff_append_props(AGoalProps,NGP)
	).
%% Else, argument is assumed to be simply a term
get_arg_props(PDA,PDA,D-D,C-C,A-A,G-G,_NPD,_F,_A,_Opts,_AType).

resolve_applications([],[]) :- 
	!.
%% newer ciao versions translate T(X,Y) to call(T,X,Y) instead.
%% resolve_applications([apply(CF,[Arg])|R],[Prop|NR]) :-
%% 	!,
%% 	CF =.. [PF|FArgs],
%% 	Prop =.. [PF,Arg|FArgs],
%% 	resolve_applications(R,NR).
resolve_applications([Call|R],[Prop|NR]) :-
	nonvar(Call),
	Call =.. [call,CF|Args],
	!,
	errs :: errlog <- ~get_errs,
	(  nonvar(CF)
	-> CF =.. [PF|FArgs],
	   %% we take care of call(foo(X),Y)
	   %PBC Wrong: append(FArgs,Args,AllArgs), 
	   apply(Args,FArgs,AllArgs), 
	   %% we take care recursively of nesting: call(foo,X,call(bar,Y))
	   resolve_applications(AllArgs,AllArgsResolved),
	   Prop =.. [PF|AllArgsResolved]
	; errs.compiler_error(mode_not_sufficiently_instantiated(Call))
        ),
	resolve_applications(R,NR).
resolve_applications([Prop|R],[NProp|NR]) :-
	nonvar(Prop),
	!,
	Prop =.. [Functor|Args],
	resolve_applications(Args,ArgsResolved),
	NProp =.. [Functor|ArgsResolved],
	resolve_applications(R,NR).
resolve_applications([Prop|R],[Prop|NR]) :-
	resolve_applications(R,NR).
}.

%% with no argument variable, e.g., p(+), p(in(foo))
with_or_without_arg(PDA,NPDA,Prop) :-
        ground(PDA),
	!,
 	PDA =.. [F|Rest],
 	Prop =.. [F,NPDA|Rest].
%% with argument variable, e.g., p(+(X)), p(in(X,foo))
with_or_without_arg(PDA,NPDA,Prop) :-
   	PDA =.. [_,NPDA|Rest],
	var(NPDA),
	ground(Rest),
	!,
 	Prop = PDA.

diff_append_props([],T-T).
diff_append_props([H|T],PH-PT) :-
	PH=[H|NPT],
	diff_append_props(T,NPT-PT).

apply([],Args,Args).
apply([A|Args],FArgs,[A|AllArgs]):-
	append(FArgs,Args,AllArgs).

%% ---------------------------------------------------------------------------
{
:- extends modread_ctx.

:- doc(norm_arg_props/4,"@var{Props} is a term describing
     properties in an assertion call or sucess point. @var{PropExpr}
     is the normalized version of @var{Props} in list format. ").

:- pred norm_arg_props(Props,PropExpr,PD,Arity) 
   :  (property_conjunction(Props),var(PropExpr),nonvar(PD),int(Arity)) 
   => nonvar(PropExpr).

:- pred norm_arg_props(Props,PropExpr,PD,Arity) 
   :  (property_starterm(Props),var(PropExpr),nonvar(PD),int(Arity)) 
   => nonvar(PropExpr).

% No props
norm_arg_props(true,[],_PD,_Arity) :- !.
% No props
norm_arg_props([],[],_PD,_Arity) :- !.
% Abridged props: * main funct or single prop (arity zero or {} main functor)
norm_arg_props(Props,PropExp,PD,Arity) :-
	%% The last two are the two unary base cases (hardest to detect)
	( Props = _R * _L ; Props = '{}'(_) ; ground(Props) ),
 	!,
	norm_abridged_props(Props,PropExp,PD,Arity).
% Normal props (conjucntion)
norm_arg_props(Props,PropExp,_PD,_Arity) :-
 	norm_normal_props(Props,PropExp).
	
% No disjunctions supported yet... 
norm_normal_props((Prop,Rest),[Prop|NRest]) :- !,
	norm_normal_props(Rest,NRest). 
norm_normal_props(FinalProp,[FinalProp]).

norm_abridged_props(Ps * P,NPs,PD,Arg) :-
	add_argvars(P,Arg,NArg0,PD,NP),
	NArg is NArg0 - 1,
	norm_abridged_props(Ps,NLs,PD,NArg), !,
	append(NLs,NP,NPs).
norm_abridged_props(P,NP,PD,Arg) :-
	add_argvars(P,Arg,NArg0,PD,NP),
	% check the rest of args are nonvar:
	NArg is NArg0 - 1,
	\+ add_argvars(P,NArg,_,PD,NP), !.
norm_abridged_props(_P,_NP,PD,_Arg) :-
	functor(PD, F, A),
	errs :: errlog <- ~get_errs,
	errs.compiler_error(arity_mismatch(F, A)).
}.

add_argvars('{}'(P),Arg,NArg,PD,NPs) :- !,
	add_tuple_argvars(P,Arg,NArg,PD,NPs).
add_argvars(P,Arg,NArg,PD,[NP]) :- 
	add_argvar(P,Arg,NArg,PD,NP).

add_tuple_argvars(','(P,PR),Arg,NArg,PD,[NP|NPs]) :- !,
	add_argvar(P,Arg,NArg,PD,NP),
	% all refered to the same NArg:
	add_tuple_argvars(PR,NArg,NArg,PD,NPs).
add_tuple_argvars(P,Arg,NArg,PD,[NP]) :-
	add_argvar(P,Arg,NArg,PD,NP).

add_argvar(M:P,Arg,NArg,PD,M:NP) :- !,
	add_argvar(P,Arg,NArg,PD,NP).
add_argvar(P,Arg,NArg,PD,NP) :-
	arg(Arg,PD,Var),
	var(Var), !,
	NArg = Arg,
	P =.. [F|Vars],
	NP =.. [F,Var|Vars].
add_argvar(P,Arg,NArg,PD,NP) :-
	NArg1 is Arg-1,
	NArg1 > 0,
	add_argvar(P,NArg1,NArg,PD,NP).

%% ---------------------------------------------------------------------------
%:- public norm_goal_prop/3.
:- doc(norm_goal_props(Props,PropList,NPr), "@var{Props} is a
   term describing global properties in an assertion. The standard
   format is a conjunction of 0-ary (or, possibly unary)
   properties. @var{PropList} is the normalized version of @var{Props}
   in list format.").

:- pred norm_goal_props(+Props,-PropList,-NPr) # "Normalizes global
   properties.".
:- pred norm_goal_props(-Props,+PropList,+NPr) # "Denormalizes global
   properties.".

%% Needs to be improved?
% TODO: use conj_to_list
norm_goal_props(true,[],_) :- !.
norm_goal_props((GP,GPs),[NGP|NGPs],NPD) :- !,
	norm_goal_prop(GP,NGP,NPD),
	norm_goal_props(GPs,NGPs,NPD).
norm_goal_props(GP,[NGP],NPD) :- !,
	norm_goal_prop_(GP,NGP,NPD).

norm_goal_prop_(M:GP,M:NGP,NPD):- !,
	norm_goal_prop(GP,NGP,NPD).
norm_goal_prop_(GP,NGP,NPD):-
	norm_goal_prop(GP,NGP,NPD).

%% Univ is not smart enough for one version
norm_goal_prop(GP,NGP,NPD) :- nonvar(GP), !,
	GP  =.. [F|Args],
	NGP =.. [F,NPD|Args].
norm_goal_prop(GP,NGP,NPD) :- nonvar(NGP), !,
	NGP =.. [F,NPD|Args],
	GP  =.. [F|Args].

%:- public denorm_goal_prop/3. % used in ciaopp
denorm_goal_prop(NGP,GP,NPD) :-
	norm_goal_prop(GP,NGP,NPD).

