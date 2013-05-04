:- doc(section, "Annotated Terms").

% TODO: Merge both versions (see psymbol below)

% TODO: Define as a class and make the rest inherit from it.
% TODO: Define as a algebraic data type? 
:- public static aterm/1.
:- regtype aterm(X) # "@var{X} is an annotated term.".
aterm(_).

% Representation of variables
:- public class termvar {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Define instance_of__/1 automatically?
    :- public static meta_predicate instance_of__(out(termvar)).
    instance_of__(X) :- var(X), !, fail.
    instance_of__('$v'(_)).
    instance_of__('$v'(_, _)).

    :- constructor new_/0.
    new_ :- ~self = '$v'(_).

    :- constructor new_n_/1.
    new_n_(N) :- ~self = '$v'(N).

    :- public name/1.
    name := N :- ~self = '$v'(N), !.
    name := N :- ~self = '$v'(N, _), !.

    % TODO: Not used in JS backend.
    :- public getp/2.
    getp(X, Y) :-
        ~self = '$v'(_, Props),
	mymember(Props, X, Y).

    % TODO: Not used in JS backend.
    :- public meta_predicate addp(?, ?, out(termvar)).
    addp(X, Y) := '$v'(N, a(X, Y, n)) :- ~self = '$v'(N), !.
    addp(X, Y) := '$v'(N, a(X, Y, Props2)) :- ~self = '$v'(N, Props), !,
	( myremove(Props, X, Props2) -> true ; Props2 = Props ).

    :- public meta_predicate delp(?, out(termvar)).
    delp(X) := '$v'(N, Props) :- ~self = '$v'(N, Props0), !,
	myremove(Props0, X, Props), !.
}.

% Representation of structures
:- public class termstr {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Define instance_of__/1 automatically?
    :- public static meta_predicate instance_of__(out(termstr)).
    instance_of__(X) :- var(X), !, fail.
    instance_of__('$s'(_, _, _)).
    instance_of__('$s'(_, _, _, _)).

:- if(use_backend(bc)).
    :- constructor new_n_/2.
    new_n_(N/A, Args) :- ~self = '$s'(N, A, Args).
    :- constructor new_f_/1.
    new_f_(N/Arity) :- ~self = '$s'(N, Arity, ~new_vars(Arity)).
:- elif(use_backend(js)).
    :- constructor new_prim_/1.
    new_prim_(N) :- ~self = '$s'(N, 0, []).
    :- constructor new_psym_/2.
    new_psym_(PredId, Args) :-
	trust(PredId instance_of psymbol),
	A = ~psymbol_arity(PredId),
        ~self = '$s'(PredId, A, Args).
    :- constructor new_f_/1.
    new_f_(N/Arity) :- ~self = '$s'(N, Arity, ~new_vars(Arity)).
:- endif.

    :- public args/1.
    args := As :- ~self = '$s'(_, _, As), !.
    args := As :- ~self = '$s'(_, _, As, _), !.
    :- public set_args/2.
    set_args(Args) := '$s'(N, A, Args) :- ~self = '$s'(N, A, _), !.
    set_args(Args) := '$s'(N, A, Args, Props) :- ~self = '$s'(N, A, _, Props), !.

:- if(use_backend(bc)).
    :- public name/1.
    name := N/A :- ~self = '$s'(N, A, _), !.
    name := N/A :- ~self = '$s'(N, A, _, _), !.
    :- public set_name/2.
    set_name(N/A) := '$s'(N, A, Args) :- ~self = '$s'(_, _, Args), !.
    set_name(N/A) := '$s'(N, A, Args, Props) :- ~self = '$s'(_, _, Args, Props), !.
:- elif(use_backend(js)).
%    :- public name/1.
%    name := N/A :- ~self = '$s'(N, A, _), !.
%    name := N/A :- ~self = '$s'(N, A, _, _), !.
%    :- public set_name/2.
%    set_name(N/A) := '$s'(N, A, Args) :- ~self = '$s'(_, _, Args), !.
%    set_name(N/A) := '$s'(N, A, Args, Props) :- ~self = '$s'(_, _, Args, Props), !.
    :- public predid/1.
    % TODO: not always a 'psymbol' (i.e., for numbers)
    predid := N :- ~self = '$s'(N, _, _), !.
    predid := N :- ~self = '$s'(N, _, _, _), !.
:- endif.

    :- public get_functor/2.
    get_functor(N, A) :- ~self = '$s'(N, A, _), !.
    get_functor(N, A) :- ~self = '$s'(N, A, _, _), !.

    :- public arity/1.
    arity := Arity :- get_functor(_, Arity).

    % TODO: Not used in practice
    :- public getp/2.
    getp(X, Y) :-
        ~self = '$s'(_, _, _, Props),
	mymember(Props, X, Y).

    % TODO: Not used in practice
    :- public addp/3.    
    :- meta_predicate addp(?, ?, out(termstr)).
    addp(X, Y) := '$s'(N, A, Args, a(X, Y, n)) :- ~self = '$s'(N, A, Args), !.
    addp(X, Y) := '$s'(N, A, Args, a(X, Y, Props2)) :- ~self = '$s'(N, A, Args, Props), !,
	( myremove(Props, X, Props2) -> true ; Props2 = Props ).
}.

% ===========================================================================

:- public new_vars/2.
new_vars(0) := [] :- !.
new_vars(N) := [~termvar.new | ~new_vars(N1)] :- N1 is N - 1.

:- if(use_backend(bc)).
:- elif(use_backend(js)).
% (like new_vars/2, but uses my_var_new)
:- public new_vars_m/2.
new_vars_m(0) := [] :- !.
new_vars_m(N) := [~my_var_new(_,_) | ~new_vars_m(N1)] :- N1 is N - 1.

:- public my_var_new/3.
my_var_new(D, Mem) := V :-
	V0 = ~termvar.new,
	V1 = ~V0.addp(d, D),
	V = ~V1.addp(mem, Mem).
:- endif.

% ===========================================================================

% Representation of goals
:- public class strgoal {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

:- if(use_backend(bc)).
    :- constructor new_n_/2.
    new_n_(PredId, Args) :-
        ~self = '$g'(PredId, Args).

    :- constructor new_f_/1.
    new_f_(PredId) :-
	trust(PredId instance_of predicate_x),
	NA = ~PredId.name, NA = _/A,
        ~self = '$g'(PredId, ~new_vars(A)).

    :- public meta_predicate predid(out(predicate_x)).
    predid := PredId :-
        ( ~self = '$g'(PredId, _) -> true
	; ~self = '$g'(PredId, _, _)
	).

    :- public name/1.
    name := Name :-
        PredId = ~predid,
	Name = ~PredId.name.
:- elif(use_backend(js)).
    :- constructor new_psym_/2.
    new_psym_(PredId, Args) :-
        ~self = '$g'(PredId, Args).

    :- constructor new_f_/1.
    new_f_(PredId) :-
	trust(PredId instance_of psymbol),
	A = ~psymbol_arity(PredId),
        ~self = '$g'(PredId, ~new_vars(A)).

    :- public meta_predicate predid(out(psymbol)).
    predid := PredId :-
        ( ~self = '$g'(PredId, _) -> true
	; ~self = '$g'(PredId, _, _)
	).

    :- public get_id/1.
    get_id := Id :-
        ~self = '$g'(PredId, _),
	PredId instance_of predicate_s,
	!,
        Id = ~PredId.get_id.
    get_id := '<not_predicate_s>'. % TODO: wrong
:- endif.

    :- public args/1.
    args := As :-
        ( ~self = '$g'(_, As) -> true
	; ~self = '$g'(_, As, _)
	).

    :- public meta_predicate set_args(?, out(strgoal)).
    set_args(Args) := '$g'(NA, Args) :- ~self = '$g'(NA, _), !.
    set_args(Args) := '$g'(NA, Args, Props) :- ~self = '$g'(NA, _, Props).

    :- public meta_predicate set_predid(?, out(strgoal)).
    set_predid(PredId) := '$g'(PredId, Args) :- ~self = '$g'(_, Args), !.
    set_predid(PredId) := '$g'(PredId, Args, Props) :- ~self = '$g'(_, Args, Props).

    :- public getp/2.
    getp(X, Y) :-
        ~self = '$g'(_, _, Props),
	mymember(Props, X, Y).

    :- public meta_predicate addp(?, ?, out(strgoal)).
    addp(X, Y) := '$g'(NA, Args, a(X, Y, n)) :- ~self = '$g'(NA, Args), !.
    addp(X, Y) := '$g'(NA, Args, a(X, Y, Props2)) :- ~self = '$g'(NA, Args, Props), !,
	( myremove(Props, X, Props2) -> true ; Props2 = Props ).
}.

% TODO: Give a different name to 'termunk'. It is used to wrap special
%   terms, like inline C code in the ptoc backend, memory values, etc.
%
% TODO: Important: there is a declaration in 'check_test_str' which
%   states that it has a unk arg, but the code generated for that
%   builtin does not have a unknown arg there.

:- public class termunk {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Define instance_of__/1 automatically.
    :- public static meta_predicate instance_of__(out(termunk)).
    instance_of__(X) :- var(X), !, fail.
    instance_of__('$u'(_, _)).

    :- constructor new_/1.
    new_(Value) :- ~self = '$u'(Value, []).

    :- public value/1.
    :- pred value(-Value) :: term # "Obtain the associated value.".
    value := Value :- ~self = '$u'(Value, _).
}.

% ---------------------------------------------------------------------------

:- public static aterm_vars/2.
% :- pred aterm_vars(+aterm, -list(termvar)) 
%    # "Collect all variables in the aterm X. Variables may be repeated.".
aterm_vars(X) := Ys :-
	vars :: accum(Ys),
	aterm_vars__2(X).
{
:- fluid vars :: accum.
aterm_vars__2(X) :- X instance_of termvar, !, vars.add(X).
aterm_vars__2(X) :-
	trust(X instance_of termstr),
	aterm_vars__3(~X.args).

aterm_vars__3([]).
aterm_vars__3([X|Xs]) :- aterm_vars__2(X), aterm_vars__3(Xs).
}.

% ---------------------------------------------------------------------------
% Auxiliary

:- static mymember/3.
mymember(a(X0, Y0, Xs), X, Y) :-
	( X = X0 ->
	    Y = Y0
	; mymember(Xs, X, Y)
	).

:- static myremove/3.
myremove(a(X0, Y, Xs0), X) := Xs :-
	( X0 = X ->
	    Xs = Xs0
	; Xs = a(X0, Y, Xs1),
	  Xs1 = ~myremove(Xs0, X)
	).
