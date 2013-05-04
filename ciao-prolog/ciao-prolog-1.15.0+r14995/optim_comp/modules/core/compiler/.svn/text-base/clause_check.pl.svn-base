% ===========================================================================

:- doc(section, "Clause sanity checks").

% This module implements a checker to detect some conditions that
% frequently are identified with programming mistakes:
%
%   - multiarity predicates
%   - discontiguous clauses
%   - singleton variables
%
% TODO: Transform this into a real package (compilation module).
%       Make sure this change does not introduce a big overhead.
%       Add new hooks if necessary.
%
% ===========================================================================

% PROLOG FLAGS
% TODO: this prolog_flag stuff is not THREAD SAFE
:- multifile define_flag/3.
define_flag(single_var_warnings, [on,off], on).
define_flag(discontiguous_warnings, [on,off], on).
define_flag(multi_arity_warnings, [on,off], on).

% ---------------------------------------------------------------------------

:- doc(subsection, "Declarations").

% ---------------------------------------------------------------------------
% discontiguous/1 - Declare a predicate as discontiguous

% TODO: Those are not exactly symbol modifiers, since no predicate
%   is actually defined.

symmodif__def(discontiguous).
symmodif__set(discontiguous, F/A) :- !,
	Pred = ~pred_ref_noreg_ac(F, A),
	modreadEnv.add1_discontiguous(~Pred.get_id).

% ---------------------------------------------------------------------------

:- doc(subsection, "Other hooks").

{
:- extends modread_ctx.
clause_check(Pred, Sings) :-
	trust(Pred instance_of predicate_s),
	( is_latest_read_pred(Pred) ->
	    % No check necessary
	    true
	; multi_arity_check(Pred),
	  discontiguous_check(Pred),
	  % Set current reading predicate
	  set_latest_read_pred(Pred)
	),
	singleton_check(Pred, Sings).
}.

% ---------------------------------------------------------------------------
% Checks done once per each chunk of clauses

{
:- extends errlog_ctx.
% TODO: use find_arities, which is a more high-level definition
multi_arity_check(Pred) :-
	trust(Pred instance_of predicate_s),
        current_prolog_flag(multi_arity_warnings, on),
	\+ Pred.get_prop(exported), % Pred not exported
	% There is other predicate with the same name, different arity,
	% and that is not being exported
	% TODO: this could be done after all the code has been read
	Pred2 = ~Pred.arity_family(A2), A2 =\= ~Pred.a,
	\+ Pred2.get_prop(exported),
	!,
        ft_error(already_defined(~Pred.name_spec, A2)).
multi_arity_check(_).
}.

{
:- extends modread_ctx.
discontiguous_check(Pred) :-
	trust(Pred instance_of predicate_s),
        current_prolog_flag(discontiguous_warnings, on),
	Pred.get_prop(has_clauses), % the predicate has been read before
        \+ modreadEnv.get_discontiguous(~Pred.get_id),
        ft_error(discontiguous(~Pred.name_spec)), !.
discontiguous_check(_).
}.

% ---------------------------------------------------------------------------
% Latest read pred

{
:- extends modread_ctx.
is_latest_read_pred(Pred) :-
	trust(Pred instance_of predicate_s),
	modreadEnv.get_latest_read_pred(~Pred.get_id).

set_latest_read_pred(Pred) :-
	trust(Pred instance_of predicate_s),
	modreadEnv.del_latest_read_pred(_),
	modreadEnv.add_latest_read_pred(~Pred.get_id).
}.

% ---------------------------------------------------------------------------

{
:- extends errlog_ctx.
singleton_check(Pred, Singletons) :-
        current_prolog_flag(single_var_warnings, on), !,
        singleton_check__no_underlines(Singletons, BadSingletons),
        singleton_check__2(BadSingletons, Pred).
singleton_check(_, _).

singleton_check__2([], _) :- !.
singleton_check__2(BadSingletons, Pred) :-
	trust(Pred instance_of predicate_s),
	ft_error(bad_singletons(BadSingletons, ~Pred.name_spec)).
}.

singleton_check__no_underlines([], []).
singleton_check__no_underlines([N=_|Eqs], Ns) :-
        ( atom_concat('_', _, N) ->
            singleton_check__no_underlines(Eqs, Ns)
        ; Ns = [N|Ns_],
          singleton_check__no_underlines(Eqs, Ns_)
        ).

