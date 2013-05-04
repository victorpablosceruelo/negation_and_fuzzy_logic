:- doc(section, "Scopes for Dynamic Binding").
% Support for dynamic binding (vs. lexical) of symbols.
% TODO: rename this part; it talks about implicit arguments and effects
%
% Note: currently, the only compilation mode flattens the dynamic
%       environment as implicit arguments.

% ---------------------------------------------------------------------------
% Fluid variables (attributes dynamically bound to the dynamic environment)

% TODO: normalize and check for errors before processing
symdecl__treatDom((fluid _)) :- get_pragma(class_expand), !.
symdecl__treat((fluid Name :: Class + Opts # _Doc)) :- !, do_fluid(Name, Class, Opts).
symdecl__treat((fluid Name :: Class + Opts)) :- !, do_fluid(Name, Class, Opts).
symdecl__treat((fluid Name :: Class # _Doc)) :- !, do_fluid(Name, Class, none).
symdecl__treat((fluid Name :: Class)) :- !, do_fluid(Name, Class, none).
symdecl__symspec((fluid Name :: _ + _ # _), Name) :- !.
symdecl__symspec((fluid Name :: _ + _), Name) :- !.
symdecl__symspec((fluid Name :: _ # _), Name) :- !.
symdecl__symspec((fluid Name :: _), Name) :- !.

{
:- extends modread_ctx.
do_fluid(Name, Class, Opts) :-
	atom(Class),
	Module = ~envmod,
	trust(Module instance_of module_s),
	% TODO: check that extending Module is possible
	Module.incctx(fluidsig(Name, Class, Opts, no)).
}.

% ---------------------------------------------------------------------------
% Declarations for 'binder'

% TODO: use 'export', treat binders as special predicates
decl__treatDom('$export_binder'(_)).
decl__treat('$export_binder'(Spec)) :- !, nonvar(Spec), do_export_binder(Spec).

{
:- extends modread_ctx.
do_export_binder(FA) :- nonvar(FA), FA = F/A, atom(F), integer(A), !,
        (~def_envmod).add1_exported_binder(F, A).
do_export_binder(Spec) :-
        ft_error(bad_export(Spec)).
}.

% ---------------------------------------------------------------------------
% '$def_binder'/2 - defines a 'binder'
%
% (a special kind of metapredicate, which is always unfolded, and
% whose context coincides with that of the input goal).

decl__treatDom('$def_binder'(_, _)).
decl__treat('$def_binder'(Head, Body)) :- !,
	% TODO: check also Def
        ( nonvar(Head) -> true
        ; ft_error(bad_binder_head(Head))
        ),
	(~def_envmod).add1_binder_def(Head, Body).
	  
% ---------------------------------------------------------------------------
% '$context'/2 - sets the context for a given predicate
%
% TODO: Rename 'module' by 'caller_module'.
% TODO: Do not pass 'top_envmod', but 'envmod'.
% TODO: Used for the special 'module' context (equivalent to addmodule
%       meta_predicate); find a better way to do it (probably, extending 
%       with a hard-wired 'caller_module' mixin).

decl__treatDom('$context'(_, _)).
decl__treat('$context'(N/A, Context)) :- !,
	( atom(Context) -> true % TODO: better check?
        ; ft_error(bad_predicate_context(N, A, Context))
	),
	Pred = ~pred_ref_ac(N, A),
	Pred.set_prop(owner_module_ctx(Context)).

% A context modifier 
decl__treatDom('$ctxprj'(_, _)).
decl__treat('$ctxprj'(N/A, Prj)) :- !,
	Pred = ~pred_ref_ac(N, A),
	Pred.set_prop(context_prj(Prj)).

% Mark that in the current scope all predicates will be static
% TODO: only work inside open-anon enclosed by the top module
decl__treatDom('$all_static').
decl__treat('$all_static') :- !,
	M = ~envmod,
	trust(M instance_of module_s),
	M.set_prop(static_scope).



