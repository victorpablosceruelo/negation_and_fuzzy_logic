:- module(_, [], [compiler(complang)]).

:- ensure_loaded(toplevel(toplevel__scope)).
:- import(user, ['$toplevel_module'/1, '$toplevel_call'/1]).

% Include all compile capabilities
:- use_module(compiler(all_actions), []).

:- use_module(compiler(dynload), [use_module/3, ensure_loaded/2]).
:- use_module(compiler(read_source)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(errhandle)).
:- use_module(library(ttyout)).
:- use_module(library(write), [write/1, write_term/3]).
:- use_module(library(operators), [op/3]).
:- use_module(library(sort), [sort/2, keysort/2]).
:- use_module(library(attrdump), [copy_extract_attr/3]).
:- use_module(library(debugger)).
:- use_module(engine(streams_basic), ['$open'/3]).
:- use_module(engine(internals), 
        ['$bootversion'/0, 
         '$abolish'/1,'$empty_gcdef_bin'/0]).
:- use_module(engine(hiord_rt), [call/1]).
:- use_module(library(lists), [difference/3]).
:- use_module(library(format), [format/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library(dict), [dic_lookup/3, dic_get/3]).

:- multifile exit_hook/0, after_query_hook/0, after_solution_hook/0.

:- multifile define_flag/3.

define_flag(prompt_alternatives_no_bindings, [on,off], off).

:- data toplevel_module/1. % Module where queries are called

:- export(toplevel/1). % (not a top-level command)
toplevel(Args) :-
	reset_debugger(_),
        get_alias_path,
        '$toplevel_module'(Module),
        asserta_fact(toplevel_module(Module)),
	interpret_args(Args, true),
        displayversion,
        op(900,fy,[(spy),(nospy)]),
        toplevel_body,
	( exit_hook, fail ; true).

% TODO: include_if_exists is not very clean...
interpret_args([], Load_CiaoRC) :- !,
	( Load_CiaoRC = true -> 
            include_if_exists('~/.ciaorc')
        ;
	    true
	).
interpret_args(['-f'|R], _) :- !, % fast start
	interpret_args(R, false).
interpret_args(['--version'], _) :- !,
        '$bootversion', % Display Ciao version
         halt.
interpret_args(['-l',File|R], _) :- !,
        include_if_exists(File),
	interpret_args(R, false).
interpret_args(['-u',File|R], Load_CiaoRC) :- !,
        use_module(File),
	interpret_args(R, Load_CiaoRC).
interpret_args(['-p',Prompt|R], Load_CiaoRC) :- !,
	top_prompt(_, Prompt),
	interpret_args(R, Load_CiaoRC).
interpret_args(['-g',Goal|R], Load_CiaoRC) :- !, % TODO: we need atom to term
	'$toplevel_call'(Goal),
	interpret_args(R, Load_CiaoRC).
interpret_args(_WinMesh, _) :-
        get_os('Win32'), !, % For windows shortcuts
        include_if_exists('~/.ciaorc').
interpret_args(_Args, _) :-
        display('Usage: ciaosh [-f] [-l <File>] [-u <File>] [-p <Prompt>]'),
        nl,
        halt(1).

include_if_exists(File) :-
        ( file_exists(File) ->
            include(File)
        ; prolog_flag(quiet, QF, warning),
	  default_toplevel_package(Package),
          use_package(Package),
          prolog_flag(quiet, _, QF) 
        ).

default_toplevel_package(default_for_ciaosh).

:- export('$toplevel_abort'/0). % (not a top-level command)
'$toplevel_abort' :-
	message('{ Execution aborted }'),
	reset_debugger(_),
	toplevel_body,
	( exit_hook, fail ; true).

toplevel_body :-
        intercept(top_toplevel_env,
                  control_c,
                  do_interrupt_command(0'\n)).

top_toplevel_env :-
        reset_query_level,
        catch(toplevel_env(_Vars), go_top, top_toplevel_env).

toplevel_env(Vars) :-
        repeat,
        toplevel_query(Vars, Query),
	( after_query_hook, fail ; true ),
	Query == end_of_file,
	!.

:- push_prolog_flag(multi_arity_warnings,off).

:- data top_prompt/1.

:- pop_prolog_flag(multi_arity_warnings).

:- data top_prompt_base/1.

top_prompt_base('?- ').

:- export(top_prompt/2). % (not a top-level command)
% Actually, sets top_prompt_base, but since seen externally, used simpler name
top_prompt(Old, New) :-
	retract_fact(top_prompt_base(Old)),
	asserta_fact(top_prompt_base(New)).

toplevel_query(Variables, Query) :-
	% TODO: unsafe... what if some thread is still running?
        '$empty_gcdef_bin', % Really get rid of abolished predicates
        debugger_info,
        current_fact(top_prompt(TP)),
        prompt(Prompt, TP),
	( true ; prompt(_, Prompt), fail),
	get_query(Query, Variables),
        prompt(_, Prompt),
        !,
        ( Query == top ->
            throw(go_top)
        ; valid_solution(Query, Variables)
	).
toplevel_query(_Variables, end_of_file).

debugger_info :- 
        ( get_debugger_state(State),
	  arg(1, State, T),
	  \+ T = off ->
	    ttydisplay('{'),ttydisplay(T),ttydisplay('}\n')
	; true
	).

get_query(Query, Dict) :-
	dynload:get_memo(Memoize),
	trust(Memoize instance_of memoize), % TODO: this should not be necessary
	Errs = ~Memoize.errs,
	Stream = user,
	call((
          errs :: any, ~errs = Errs,
	  stream :: any, ~stream = Stream,
	  read_query(RawQuery, Dict, VarNames)
        )),
	Errs.clear,
        Query \== up,
        syntr__expand_query(RawQuery, VarNames, Query).

toplevel_debug_call(Query, MoreSols) :-
        ( adjust_debugger ; switch_off_debugger, fail),
        '$metachoice'(BeforeChoice),
        '$toplevel_call'(Query),
        '$metachoice'(AfterChoice),
        MoreSols = ( BeforeChoice = AfterChoice ? false | true ),
        ( switch_off_debugger ; adjust_debugger, fail).

valid_solution(Query, Variables) :-
	% TODO: the alternative is catching exception and returning to the top, but IMHO that is worse (you can write X is a by mistake and do not want to lose query level) -- jfran
        ( catch((toplevel_debug_call(Query, MoreSols), Result = yes), 
	        E, (Result = exception(E)))
	; Result = no
	),
	( Result = yes ->
	    ( after_solution_hook, fail ; true ),
	    % display and validate solution
	    uncycle_dict(Variables, VariablesNoCycles),
	    answer_constraints(VariablesNoCycles, Dict, Constraints),
	    call(( eqs :: accum(Eqs), solution_vars(Dict) )),
	    prettyvars([Eqs|Constraints]),
	    display_solution(Eqs, Constraints, MoreSols, Ask),
	    validate_solution(Ask, Eqs, Constraints, Variables),
	    !, % we do not want more solutions
	    fsep(yes)
	; Result = no ->
	    !,
	    % display no solution
	    fsep(no)
	; Result = exception(E) ->
	    !,
	    % TODO: plug here runtime errors display (or in the previous catch)
	    % display error
	    default_error_message(E),
	    fsep(aborted)
	).

{
:- fluid eqs :: accum.
solution_vars(D) :- var(D), !.
solution_vars(dic(Var,[Val|_],L,R)) :-
 	solution_vars(L),
        solution_var(Var, Val),
	solution_vars(R).

%solution_var([0'_|_], _) :- !.  % Do not display vars starting with "_"
solution_var(Var, Val) :- var(Val), !,
        atom_codes(AtomVar, Var), Val='$VAR'(AtomVar).
solution_var(Var, Val) :-
        eqs.add(Var = Val).
}.

% ---------------------------------------------------------------------------

uncycle_dict(Dict, NewDict) :-
        dict_rev_queue_eq(Dict, RevDict, EqList, EqListTail),
        % Dict and the 0 are used for generating new variable names
        uncycle_eqs(EqList, EqListTail, 0, Dict, RevDict, NewDict).

dict_rev_queue_eq(Var, _, L, L_) :- var(Var), !, L_ = L.
dict_rev_queue_eq(dic(Var,[Val|_],Lft,Rgt), RD, L, L_) :-
        ( Var = "_"||_ ->
            % Vars starting with "_" are not used
            L = L1
        ;
            dic_lookup(RD, Val, VarX),
            ( var(VarX) -> VarX = Var ; true ),
            L = [(Var,Val)|L1]
        ),
        dict_rev_queue_eq(Lft, RD, L1, L2),
        dict_rev_queue_eq(Rgt, RD, L2, L_).

uncycle_eqs(EqL, EqLTail, _N, _Dict, _RevDict, _NewDict) :-
        EqL == EqLTail, !.
uncycle_eqs([(Var,Val)|EqL], EqLTail, N, Dict, RevDict, NewDict) :-
        uncycle_val(Val, [], N, N1, Dict, RevDict, EqLTail, EqLTail_, NewVal),
        dic_lookup(NewDict, Var, [NewVal]),
        uncycle_eqs(EqL, EqLTail_, N1, Dict, RevDict, NewDict).

uncycle_val(Val, _Seen, N, N1, _Dict, _RevDict, NewEqs, NewEqs_, NewVal) :-
        var(Val), !,
        N1 = N, 
        NewEqs = NewEqs_,
        NewVal = Val.
uncycle_val(Val, _Seen, N, N1, _Dict, _RevDict, NewEqs, NewEqs_, NewVal) :-
        atomic(Val), !,
        N1 = N, 
        NewEqs = NewEqs_,
        NewVal = Val.
uncycle_val(Val, Seen, N, N1, Dict, RevDict, NewEqs, NewEqs_, NewVal) :-
        already_seen(Seen, Val), !,
        dic_lookup(RevDict, Val, Var),
        ( var(Var) ->
            new_varname(N, Dict, Var, N1),
            NewEqs = [(Var,Val)|NewEqs_]
        ;
            N1 = N,
            NewEqs_ = NewEqs
        ),
        atom_codes(VarName, Var),
        NewVal = '$VAR'(VarName).
uncycle_val(Val, Seen, N, N1, Dict, RevDict, NewEqs, NewEqs_, NewVal) :-
        functor(Val, F, A),
        functor(NewVal, F, A),
        uncycle_val_args(A, Val, [Val|Seen], N, N1, Dict,
                         RevDict, NewEqs, NewEqs_, NewVal).

uncycle_val_args(0, _, _, N, N, _, _, NewEqs, NewEqs, _) :- !.
uncycle_val_args(A, Val, Seen, N, N_, Dict, RevDict, NewEqs, NewEqs_, NVal) :-
        A1 is A-1,
        arg(A, Val,  ValA),
        arg(A, NVal, NValA),
        uncycle_val(ValA, Seen, N, N1, Dict, RevDict, NewEqs, NewEqs1, NValA),
        uncycle_val_args(A1, Val, Seen, N1, N_, Dict, RevDict,
                         NewEqs1, NewEqs_, NVal).

already_seen([T|_], Term) :-
        T == Term, !.
already_seen([_|Ts], Term) :-
        already_seen(Ts, Term).

new_varname(N, Dict, Var, N_) :-
        N1 is N+1,
        number_codes(N,NS),
        Var0 = "_"||NS,
        ( dic_get(Dict, Var0, _) ->
            new_varname(N1, Dict, Var, N_)
        ;
            Var = Var0,
            N_ = N1
        ).

% ---------------------------------------------------------------------------
% This is alike the one in library(write), except that variable names
% start with "_"
% TODO: do not duplicate code

prettyvars(Term) :-
	call(( vars :: accum(Vars0), collect_vars(Term) )),
	keysort(Vars0, Vars),
	pretty_vars(Vars, 0).

{
:- fluid vars :: accum.
collect_vars(Var) :-
	var(Var), !, vars.add(Var-[]).
collect_vars([X|Xs]) :- !,
	collect_vars(X),
	collect_vars(Xs).
collect_vars(X) :-
	functor(X, _, A),
	collect_vars_(0, A, X).

collect_vars_(A, A, _) :- !.
collect_vars_(A0, A, X) :-
	A1 is A0+1,
	arg(A1, X, X1),
	collect_vars(X1),
	collect_vars_(A1, A, X).
}.

pretty_vars([], _).
pretty_vars([X,Y|Xs], N0) :-
	X==Y, !,
        name_var(X, N0),
	N is N0+1,
	pretty_vars_(Xs, X, N).
pretty_vars(['$VAR'('_')-[]|Xs], N0) :-
	pretty_vars(Xs, N0).

pretty_vars_([X|Xs], Y, N0) :-
	X==Y, !,
	pretty_vars_(Xs, Y, N0).
pretty_vars_(Xs, _, N0) :-
	pretty_vars(Xs, N0).

name_var('$VAR'(Name)-[], N) :-
        Letter is N mod 26 + 0'A,
        ( N>=26 ->
            Rest is N//26,
            number_codes(Rest, Index)
        ; Index = ""
        ),
        atom_codes(Name, [0'_ , Letter | Index]).

% ---------------------------------------------------------------------------

:- multifile dump/3. /* For clp[qr] .DCG. */

answer_constraints(Variables, Dict, Constraints) :-
        dump(Variables, Dict, Constraints), !.
answer_constraints(Variables, Dict, Constraints) :-
        copy_extract_attr(Variables, Dict, Constraints).

% Displays a solution (Ask=yes if the answer needs validation) 
display_solution(Eqs, Constraints, MoreSols, Ask) :-
        display_bindings(Eqs, yes, Empty0),
        display_constraints(Constraints, Empty0, Empty),
	( Empty = yes ->
	    % no visible bindings or constraints
	    ( current_prolog_flag(prompt_alternatives_no_bindings, on),
	      MoreSols = true ->
	        bsep(yes),
	        ttydisplay('true'),
		Ask = yes
	    ; Ask = no
	    )
	; Ask = yes
	),
	( Ask = yes -> qsep, ttyflush ; true ).

display_bindings([], Empty, Empty).
display_bindings([Var=Val|Eqs], Empty0, Empty) :-
        bsep(Empty0),
        display_string(Var),
        ttydisplay(' = '),
        write_term(user, Val, [quoted(true), portrayed(true),
                               numbervars(true), priority(699)]),
        display_bindings(Eqs, no, Empty).

display_constraints([], Empty, Empty).
display_constraints([Goal|Gs], Empty0, Empty) :-
        display_goal(Goal, Empty0, Empty1),
        display_constraints(Gs, Empty1, Empty).

display_goal(true, Empty, Empty) :- !.
display_goal((G1,G2), Empty0, Empty) :- !,
	display_goal(G1, Empty0, Empty1),
 	display_goal(G2, Empty1, Empty).
display_goal(G, Empty, no) :-
	bsep(Empty),
        write_term(user, G, [quoted(true), portrayed(true),
                             numbervars(true)]).

validate_solution(no, _, _, _) :- !. % do not ask
validate_solution(_, Eqs, Constraints, Variables) :-
        ttyget(C), ( C = 0'\n -> true ; ttyskip(0'\n) ), % ask the user
        ( C = 0'y -> true % y(es)
	; C = 0'\n -> true % end of line
        ; C = 0', ->
	    % add another question
            inc_query_level,
            toplevel_env(Variables),
            dec_query_level,
	    display_solution(Eqs, Constraints, true, Ask),
	    validate_solution(Ask, Eqs, Constraints, Variables)
        ; % another solution
	  fail
        ).

% ---------------------------------------------------------------------------
% Separators

% binding separator ('yes' for first binding, 'no' for others)
bsep(yes) :- ttynl.
bsep(no) :- ttydisplay(','), ttynl.

% question separator
qsep :- ttydisplay(' ? ').

% fsep separator
fsep(Status) :- ttynl, ttydisplay(Status), ttynl.

% ---------------------------------------------------------------------------

:- data querylevel/1.

reset_query_level :-
        retractall_fact(querylevel(_)),
        asserta_fact(querylevel(0)),
        set_top_prompt(0).

inc_query_level :-
        retract_fact(querylevel(N)),
        N1 is N+1,
        asserta_fact(querylevel(N1)),
        set_top_prompt(N1).

dec_query_level :-
        retract_fact(querylevel(N)),
        N1 is N-1,
        asserta_fact(querylevel(N1)),
        set_top_prompt(N1).

set_top_prompt(0) :- !,
        retractall_fact(top_prompt(_)),
	top_prompt_base(P),
        asserta_fact(top_prompt(P)).
set_top_prompt(N) :-
        number_codes(N, NS),
        atom_codes(NA, NS),
	top_prompt_base(P),
        atom_concat(NA, ' ', NS1),
        atom_concat(NS1, P, TP),
        retractall_fact(top_prompt(_)),
        asserta_fact(top_prompt(TP)).

% ===========================================================================
% Default declarations available from the toplevel

% TODO: Can I unify declaration processing with compiler/frontend.pl?

% ---------------------------------------------------------------------------

:- data '$current version'/1.

:- export(displayversion/0).
displayversion :-              % shall use current output
	(   '$bootversion', 
	    current_fact('$current version'(Msg)),
	    nl, write(Msg), nl,
	    fail
	;   true
	).

:- export(version/1).
version(A) :-
        nonvar(A), !,
	assertz_fact('$current version'(A)).
version(_) :- throw(error(instantiation_error,version/1-1)).

% ---------------------------------------------------------------------------
% Debug

:- export(debug_module/1).
:- redefining(debug_module/1).

:- export(debug_module_source/1).
:- redefining(debug_module_source/1).

:- export(nodebug_module/1).
:- redefining(nodebug_module/1).

% ---------------------------------------------------------------------------
% Including files in toplevel

:- data new_decl/1.

:- use_module(compiler(dynload), [get_memo/1]).
:- use_module(compiler(memoize), [enter/0, leave/0]).
:- use_module(compiler(store),
	[addr/2, find_source/3, find_package/3, eval_file/2,
	 spec_to_default_module/2]).
:- use_module(compiler(errlog)).

:- export(include/1).
include(Uspec) :- do_include(source, Uspec).

do_include(Kind, Uspec) :- !,
	dynload:get_memo(Memoize),
	trust(Memoize instance_of memoize), % TODO: this should not be necessary
	( Kind = source ->
	    store:find_source(Uspec, relpath('.'), Spec),
	    Action = include(Spec)
	; Kind = package ->
	    store:find_package(Uspec, relpath('.'), Spec),
	    Action = use_package(Spec)
	; fail
	),
	Memoize.enter,
	Errs = ~Memoize.errs,
	( Errs.protect(Action, include__1(Kind, Spec)) ->
	    true
	; true
	),
	Memoize.leave.

include__1(Kind, Spec) :-
	dynload:get_memo(Memoize),
	trust(Memoize instance_of memoize), % TODO: this should not be necessary
	Errs = ~Memoize.errs,
	Errs.show_action, % be verbose
	call((
          memo :: memoize <- Memoize,
	  ( Kind = source ->
	      eval_file(prolog_source(Spec), Name)
	  ; Kind = package ->
	      eval_file(prolog_package(Spec), Name)
	  ; fail
	  )
        )),
        '$open'(Name, r, Stream),
	call((
          errs :: any, ~errs = Errs,
	  stream :: any, ~stream = Stream,
          include__2(Kind, Spec)
        )),
        close(Stream),
	\+ Errs.get1_module_error.

{
% TODO: due to a compiler bug, it is not possible to extend from imported mixin
:- fluid errs :: errlog.
:- fluid stream :: any.
%:- extends ctx_read_source.
include__2(Kind, Spec) :-
	do_read_sentence(Sentence),
	check_include_decl(Kind, Spec, Sentence, Rest),
	repeat,
	( member(Sentence2, Rest)
	; do_read_sentence(Sentence2)
	),
	%read_term(Stream, RawData, [variable_names(VarNames), lines(L0, L1)]),
	( Sentence2 = end_of_file, ! % end loop
	; Sentence2 = sentence(RawData, VarNames, _, L0, L1),
	  syntr__expand_sentence(RawData, VarNames, Data0),
	  member(Data1, Data0),
	  ( Data1 = end_of_file, ! % end loop
	  ; interpret_data(Data1, L0, L1),
	    fail % loop
	  )
	).

do_read_sentence(Sentence) :-
	( read_sentence(Sentence) -> true
	; Sentence = end_of_file
	).
}.

% Check that packages contains the right declarations. Nothing is
% required for included source.
check_include_decl(source, _, Sentence, [Sentence]).
check_include_decl(package, Spec, Sentence, Sentences) :-
	( Sentence = sentence(Data, _, _, Ln0, Ln1),
	  Data = (:- package(M)) ->
	    Sentences = [],
	    SM = ~spec_to_default_module(Spec),
	    ( SM = M -> % Allow vars in package declarations
	        true
	    ; include_error_at(Ln0, Ln1, bad_module_name(package, M))
	    )
	; % Do not consume the sentence, it is not a valid package declaration
          Sentences = [Sentence],
	  ( Sentence = sentence(_,_,_,L0,L1) ->
	      include_error_at(L0, L1, bad_module_decl_kind(package, unknown))
	  ; include_error(bad_module_decl_kind(package, unknown))
	  )
	).

% TODO: disable: export all toplevel_directives, imports them from toplevel__scope, do toplevel_call
:- '$pragma'(allow_runtime_expansions).

interpret_data((?- Goal), _, _) :- !,
        '$toplevel_call'(Goal), !.
interpret_data((:- Decl), L0, L1) :- !,
        ( current_fact(new_decl(Decl)) ->
            true
        ; toplevel_directive(Decl) ->
            call(Decl)
        ; bad_toplevel_directive(Decl, L0, L1)
        ).
interpret_data(Clause, _, _) :-
        '$toplevel_call'(assertz(Clause)).

bad_toplevel_directive(Decl, L0, L1) :-
        functor(Decl,F,A),
	include_error_at(L0, L1, directive_not_allowed_in_toplevel(F, A)).

toplevel_directive(use_module(_)).
toplevel_directive(use_module(_,_)).
toplevel_directive(ensure_loaded(_)).
toplevel_directive(include(_)).
toplevel_directive(use_package(_)).
toplevel_directive(set_prolog_flag(_,_)).
toplevel_directive(push_prolog_flag(_,_)).
toplevel_directive(pop_prolog_flag(_)).
toplevel_directive(op(_,_,_)).
toplevel_directive(new_declaration(_,_)).
toplevel_directive(new_declaration(_)).
toplevel_directive(load_compilation_module(_)).
toplevel_directive(add_sentence_trans(_, _)).
toplevel_directive(add_term_trans(_, _)).
toplevel_directive(add_goal_trans(_, _)).
toplevel_directive(multifile(_)).

:- export(use_module/1).
use_module(M) :-
        current_fact(toplevel_module(Module)),
        use_module(M, all, Module).

:- export(use_module/2).
use_module(M, Imports) :-
        current_fact(toplevel_module(Module)),
        use_module(M, Imports, Module).

:- export(ensure_loaded/1).
:- redefining(ensure_loaded/1).
ensure_loaded([]) :- !.
ensure_loaded([File|Files]) :- !,
        ensure_loaded__2(File),
        ensure_loaded(Files).
ensure_loaded(File) :-
	ensure_loaded__2(File).

ensure_loaded__2(File) :-
        current_fact(toplevel_module(Module)),
	dynload:ensure_loaded(File, Module).

:- export('.'/2).
[File|Files] :-
	ensure_loaded([File|Files]).

% TODO: fix
:- export(make_exec/2).
:- redefining(make_exec/2).
make_exec(Files0, ExecName) :-
        Files = ( Files0 = [_|_] ? Files0 | [Files0] ),
	errlog:bug(fixmefixmefixme__make_exec_disabled(Files, ExecName)).
%	exemaker:make_exec(Files, ExecName).

:- export(use_package/1).
use_package([]) :- !.
use_package([F|Fs]) :- !,
        use_package(F),
        use_package(Fs).
use_package(F) :- atom(F), !,
	do_include(package, library(F)).
use_package(F) :- functor(F, _, 1), !,
	do_include(package, F).
use_package(F) :-
        include_error(bad_package_file(F)).

:- export(new_declaration/2).
new_declaration(S, _) :- new_declaration(S).

:- export(new_declaration/1).
new_declaration(S) :-
        ( S = F/A, functor(D, F, A) ->
          ( current_fact(new_decl(D)) -> true
          ; asserta_fact(new_decl(D))
          )
        ; include_error(badly_formed(new_decl(D), S))
        ).

:- export(load_compilation_module/1).
load_compilation_module(File) :-
        this_module(M),
        use_module(File, all, M),   % Here for sentence/term expansions
        current_fact(toplevel_module(ShM)),
        use_module(File, all, ShM). % In toplevel__scope for goal expansions

% ---------------------------------------------------------------------------

:- export(add_sentence_trans/2).
add_sentence_trans(P, Prior) :-
        current_fact(toplevel_module(ShMod)),
        toplevel:add_trans_hook(ShMod, sentence, P, Prior), !.
add_sentence_trans(P, Prior) :-
        include_error(declaration_failed(add_sentence_trans(P, Prior))).

% ---------------------------------------------------------------------------

:- export(add_term_trans/2).
add_term_trans(P, Prior) :-
        current_fact(toplevel_module(ShMod)),
        toplevel:add_trans_hook(ShMod, term, P, Prior), !.
add_term_trans(P, Prior) :-
        include_error(declaration_failed(add_term_trans(P, Prior))).

% ---------------------------------------------------------------------------

:- doc(subsection, "Translation hooks (term and sentence)").
% TODO: duplicated in compiler/frontend.pl

:- include(compiler(trans_hook_db)).
%     [add_trans_hook/4, del_trans_hook/1, pqueue_values/2]
:- data translation_hook/2.
get_translation_hook(Kind, KVs) :-
	current_fact(translation_hook(Kind, KVs)).
set_translation_hook(Kind, KVs) :-
	retractall_fact(translation_hook(Kind, _)),
	assertz_fact(translation_hook(Kind, KVs)).
add_translation_hook(Kind, KVs) :-
	assertz_fact(translation_hook(Kind, KVs)).
del_translation_hook(Kind) :-
	retractall_fact(translation_hook(Kind, _)).
mark_trans_hook(_).
unmark_trans_hook.

:- include(compiler(syntactic_translation)). % [syntr__expand_query/3, syntr__expand_sentence/3].

% ---------------------------------------------------------------------------

:- doc(subsection, "Translation hooks (goal)").
% TODO: duplicated in compiler/frontend.pl

% TODO: 'translation_hook/2' data is duplicated in rt_exp.pl because
%       of syntactic_translation is separated from here; merge them
:- use_module(engine(rt_exp), [add_trans_hook/4]).

:- export(add_goal_trans/2).
add_goal_trans(P, Prior) :-
        current_fact(toplevel_module(ShMod)),
        rt_exp:add_trans_hook(ShMod, goal, P, Prior), !.
add_goal_trans(P, Prior) :-
        include_error(declaration_failed(add_goal_trans(P, Prior))).

% ---------------------------------------------------------------------------

include_error(X) :-
	dynload:get_memo(Memoize),
	trust(Memoize instance_of memoize), % TODO: this should not be necessary
	Errs = ~Memoize.errs,
	Errs.compiler_error(X).

include_error_at(L0, L1, Error) :-
	% TODO: add location before?
	dynload:get_memo(Memoize),
	trust(Memoize instance_of memoize), % TODO: this should not be necessary
	Errs = ~Memoize.errs,
	Errs.add_location(L0, L1),
	Errs.compiler_error(Error),
	Errs.del_location.

% ---------------------------------------------------------------------------

:- include(toplevel(toplevel__debugger)).
:- export(consult/1).
:- export(compile/1).
:- export(display_debugged/0).


