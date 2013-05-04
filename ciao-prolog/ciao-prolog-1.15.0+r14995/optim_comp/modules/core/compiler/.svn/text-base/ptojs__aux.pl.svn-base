:- module(_, [], [compiler(complang)]).

:- doc(title, "Auxiliary definitions for pl2js").
:- doc(author, "Jose F. Morales").

% TODO: rename by 'system_mini', a minified version of system predicates
%       for use in the compiler.
% TODO: merge with versions from linker__bootstrap.

% ---------------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).

:- public copyfile/2.
copyfile(From, To) :-
	system(~atom_concat(['cp ', From, ' ', To])).

:- public copytodir/2.
copytodir(From, ToDir) :-
	system(~atom_concat(['cp ', From, ' ', ToDir])).

% ---------------------------------------------------------------------------


