:- module(open_and_protect, [open_and_protect/3, end_protect/1], [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(streams_basic)).
:- use_module(engine(data_facts)).

:- use_module(library(ctrlcclean), [delete_on_ctrlc/2]).
:- use_module(library(system), [file_exists/1, delete_file/1]).

% Open files with ctrlc protection

open_and_protect(Path, Stream, Ref) :-
        ( file_exists(Path) -> delete_file(Path) ; true ),
        delete_on_ctrlc(Path, Ref),
        '$open'(Path, w, Stream).

end_protect(Ref) :-
	erase(Ref).
