% TODO: rename __callback by __interface; document this file

% Callbacks for actions
{
:- fluid memo :: memoize.
:- static multifile action__input/2.
:- '$ctxprj'(action__input/2, [memo]).
:- discontiguous(action__input/2).
}.
% this predicate is used when the output does not exists or is outdated
{
:- fluid memo :: memoize.
:- static multifile action__do/2. 
:- '$ctxprj'(action__do/2, [memo]).
:- discontiguous(action__do/2).
}.
% TODO: document (see todo.txt)
:- static multifile action__output/2.
:- '$ctxprj'(action__output/2, []).
:- discontiguous(action__output/2).

% TODO: some actions cannot be saved or restored
:- static multifile action__save/2.
:- '$ctxprj'(action__save/2, []).
:- discontiguous(action__save/2).
:- static multifile action__restore/2.
:- '$ctxprj'(action__restore/2, []).
:- discontiguous(action__restore/2).

:- static multifile action__key/2.
:- '$ctxprj'(action__key/2, []).
:- discontiguous(action__key/2).
:- static multifile action__timestamp/2.
:- '$ctxprj'(action__timestamp/2, []).
:- discontiguous(action__timestamp/2).

:- static multifile action__terminal/1.
:- '$ctxprj'(action__terminal/1, []).
:- discontiguous(action__terminal/1).
:- static multifile action__frozen/1.
:- '$ctxprj'(action__frozen/1, []).
:- discontiguous(action__frozen/1).

% TODO: document: in-memory objects cannot be saved or restored
% TODO: for memoize those are not necessary...
:- static multifile element__save/3.
:- '$ctxprj'(element__save/3, []).
:- discontiguous(element__save/3).
:- static multifile element__restore/3.
:- '$ctxprj'(element__restore/3, []).
:- discontiguous(element__restore/3).
