:- module(_, [], [compiler(complang)]).

% TODO: move syntactic translations here? <- problem: output is a list...
% TODO: move singleton check here? <- problem: add knowledge about the semantic here...?

:- use_module(library(read)).
:- use_module(compiler(errlog)).

:- public mixin ctx_read_source {
    :- fluid errs :: errlog.
    :- fluid stream :: any.
}.

{
:- extends ctx_read_source.

:- public read_query/3.
% TODO: used in the toplevel
read_query(Query, Dict, VarNames) :-
	repeat,
	Opts = [dictionary(Dict),variable_names(VarNames)],
        catch(read_term(~stream, Query, Opts),
              error(syntax_error(L0,L1,Msg,ErrorLoc), _),
              handle_syntax_error(L0,L1,Msg,ErrorLoc)),
	!,
        Query \== end_of_file.

:- public read_sentence/1.
% :- pred read_sentence(Sentence) :: ^sentence(_,_,_,_,_)
% # "Fails if there are no more sentences to read".
read_sentence(Sentence) :-
	repeat,
	Opts = [ variable_names(VarNames),
		 singletons(Singletons),
		 lines(Ln0, Ln1) ],
        catch(read_term(~stream, Data, Opts),
	      error(syntax_error(ALn0, ALn1, Msg, ErrorLoc), _),
	      handle_syntax_error(ALn0, ALn1, Msg, ErrorLoc)),
	!,
	Data \== end_of_file,
	Sentence = sentence(Data, VarNames, Singletons, Ln0, Ln1).

:- '$ctxprj'(handle_syntax_error/4, [errs]).
handle_syntax_error(Ln0, Ln1, Msg, ErrorLoc) :-
	errs.add_location(Ln0, Ln1),
	errs.compiler_error(syntax_error(Msg, ErrorLoc)),
	errs.del_location,
	fail.
}.

