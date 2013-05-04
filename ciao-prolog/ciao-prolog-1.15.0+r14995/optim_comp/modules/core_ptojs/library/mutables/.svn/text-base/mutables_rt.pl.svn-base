:- module(mutables_rt, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Mutable terms and a collection of mutable data structures").
:- doc(author, "Jose F. Morales").

% TODO:
% - make mutable operations basal, so that mutables can be seen as
%   normal predicates
% - implement dynamic predicates
% - the difference between dynamic and mutable may be the quantifier
%   in variables!

% ===========================================================================
% TODO: I am not sure if this belongs here...

% Operators for mutable data
:- export((@)/2).
:- fun_eval((@)/1).
@A := ~A.'$mutget'.

:- export((+=)/2).
A += B :- A.'$mutinc'(B).

% ===========================================================================

% TODO: use traits or mixins?
:- class mutable_base {
    :- doc(title, "Base for mutable variables").
    % TODO: share with other mutables
    % TODO: this method may make no sense for this object
    :- export('$get_field'/2).
    '$get_field'(ChildName) := (@(~self))[ChildName].
}.

% ---------------------------------------------------------------------------

:- class nb_mut {
    :- doc(title, "Non-backtrackable mutable variables").
    :- extends mutable_base.

    :- use_package(js_lang).

    % note: nothing is done on backtracking... (it retains the nonfunctor parts)
    % note: I do not like non-backtrackable mutable variables. I prefer
    %       some kind of reflection on the emulator (like escaped
    %       un-managed code inside predicates).  I added nb_mut just for
    %       efficiency (until trailing can be done selectively).

    :- export(cons__/1).
    :- pred cons__/1 + det.
    cons__(Value) :- js_lang.stats([(~self).value <- Value]).

    :- export('$mutset'/1).
    :- pred '$mutset'/1 + det.
    '$mutset'(Value) :- js_lang.stats([(~self).value <- Value]).

    :- export('$mutinc'/1).
    '$mutinc'(Value) :- '$mutset'(~self + Value).

    :- export('$mutget'/1).
    :- pred '$mutget'/1 + detfun.
    '$mutget' := ~js_lang.expr((~self).value).
}.

% ---------------------------------------------------------------------------

:- class nb_mut_num {
    :- doc(title, "Non-backtrackable mutable variables that store numbers").
    :- extends mutable_base.

    % note: this is an example of how some operations can be implemented
    % more efficiently ('$mutinc')

    :- use_package(js_lang).

    :- export(cons__/1).
    :- pred cons__/1 :: t_num + (det, argsbox([unbox])).
    cons__(Value) :- js_lang.stats([(~self).value <- Value]).

    :- export('$mutset'/1).
    :- pred '$mutset'/1 :: t_num + (det, argsbox([unbox])).
    '$mutset'(Value) :- js_lang.stats([(~self).value <- Value]).

    :- export('$mutinc'/1).
    :- pred '$mutinc'/1 :: t_num + (det, argsbox([unbox])).
    '$mutinc'(Value) :- js_lang.stats([op('+=').[(~self).value, Value]]).

    :- export('$mutget'/1).
    :- pred '$mutget'/1 :: t_num + (detfun, argsbox([unbox])).
    '$mutget' := ~js_lang.expr((~self).value).
}.

% ---------------------------------------------------------------------------

:- class var_nb_mut_dict {
    :- doc(title, "A nonbacktrackable mutable dictionary of variables").

    % note: it uses the `get_var_id()' slot of class var
    % TODO: with JS WeakMap (FF6 only?) this could be much more efficient

    :- use_package(js_lang).

    :- export(cons__/0).
    :- pred cons__/0 + det.
    cons__ :- js_lang.stats([(~self).dict <- '{}']).

    % TODO: missing check that varkey is a var
    :- export(set/2).
    :- pred set/2 + det.
    set(Varkey, Value) :- js_lang.stats([
        (~self).dict.elem(Varkey.deref.[].get_var_id.[]) <- Value
    ]).

    % TODO: missing check that varkey is a var
    :- export(in/1).
    :- pred in/1 + semidet.
    in(Varkey) :-
        js_lang.test(in(Varkey.deref.[].get_var_id.[], (~self).dict)).

    % TODO: missing check that varkey is a var
    :- export(get/2).
    get(VarKey) := Value :- in(VarKey), !, Value = ~get_(VarKey).
    :- pred get_/2 + detfun.
    get_(Varkey) := ~js_lang.expr((~self).dict.elem(Varkey.deref.[].get_var_id.[])).

    % TODO: missing check that varkey is a var
    :- export(del/1).
    :- pred del/1 + det.
    del(Varkey) :- js_lang.stats([
        delete((~self).dict.elem(Varkey.deref.[].get_var_id.[]))
    ]).
}.

% ---------------------------------------------------------------------------

:- class string_nb_mut_dict {
    :- doc(title, "A nonbacktrackable mutable dictionary of strings").

    :- use_package(js_lang).

    :- export(cons__/0).
    :- pred cons__/0 + det.
    cons__ :- js_lang.stats([(~self).dict <- '{}']).

    :- export(set/2).
    :- pred set/2 :: t_string * term + (det, argsbox([unbox, box])).
    set(Key, Value) :- js_lang.stats([(~self).dict.elem(Key) <- Value]).

    % TODO: in/1 could be non-deterministic...
    :- export(in/1).
    :- pred in/1 :: t_string + (semidet, argsbox([unbox])).
    in(Key) :- js_lang.test(in(Key, (~self).dict)).

    :- export(get/2).
    get(Key) := Value :- in(Key), !, Value = ~get_(Key).
    :- pred get_/2 :: t_string * term + (detfun, argsbox([unbox, box])).
    get_(Key) := ~js_lang.expr((~self).dict.elem(Key)).

    :- export(del/1).
    :- pred del/1 :: t_string + (det, argsbox([unbox])).
    del(Key) :- js_lang.stats([delete((~self).dict.elem(Key))]).
}.

% ---------------------------------------------------------------------------

:- class nb_mut_array {
    :- doc(title, "A nonbacktrackable mutable array").

    :- use_package(js_lang).

    :- export(cons__/0).
    :- pred cons__/0 + det.
    cons__ :- js_lang.stats([(~self).array <- '[]']).

    :- export(set/2).
    :- pred set/2 :: t_num * term + (det, argsbox([unbox, box])).
    set(Index, Value) :- js_lang.stats([(~self).array.elem(Index) <- Value]).

    :- export(in/1).
    :- pred in/1 :: t_string + (semidet, argsbox([unbox])).
    in(Index) :- js_lang.test(in(Index, (~self).array)).

    :- export(get/2).
    get(Index) := Value :- in(Index), !, Value = ~get_(Index).
    :- pred get_/2 :: t_string * term + (detfun, argsbox([unbox, box])).
    get_(Index) := ~js_lang.expr((~self).array.elem(Index)).

    :- export(del/1).
    :- pred del/1 :: t_string + (det, argsbox([unbox])).
    del(Index) :- js_lang.stats([delete((~self).array.elem(Index))]).

    :- export(push/1).
    :- pred push/1 + det.
    push(Value) :- js_lang.stats([(~self).array.push.[Value]]).

    :- export(pop/1).
    :- pred pop/1 + detfun.
    pop := ~js_lang.expr((~self).array.pop.[]).

    :- export(length/1).
    :- pred length/1 :: t_num + (detfun, argsbox([unbox])).
    length := ~js_lang.expr((~self).array.length).

    % TODO: allow identifiers for mutables? (a debug option?)
    :- export('$to_str'/1).
    '$to_str' := "<$nb_mut_array>".
    % TODO: implement A[I]
    % TODO: optimization "A[I] <- V ===> A.set(I, V)"
 
    % TODO: not working because I do not have blocks (pred(X) :- foo(X))
    % TODO: port functional abstractions (''(X) := X + 1)
    :- export(foreach/1).
    foreach(Goal) :- foreach_(0, Goal).
    foreach_(I, Goal) :-
        ( I >= length -> true
        ; Goal(~get(I)), foreach_(I + 1, Goal)
        ).
}.

% nb_mut_array_from_list(Xs) := Array :-
% 	Array = ~nb_mut_array,
% 	nb_mut_array_from_list_(Xs, _).
% 
% nb_mut_array_from_list_([], _) :- !.
% nb_mut_array_from_list_([X|Xs], Array) :-
% 	Array.push(X),
% 	nb_mut_array_from_list_(Xs, Array).

