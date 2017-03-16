:- module(ctrlcclean, [ctrlc_clean/1, delete_on_ctrlc/2, ctrlcclean/0],
	[pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(data_facts)).
:- use_module(engine(exceptions)).
:- use_module(engine(rt_exp), [rt_modexp/4]).

:- use_module(library(system), [delete_file/1, working_directory/2]).

:- meta_predicate(ctrlc_clean(goal)).

ctrlc_clean(Goal) :- catch(Goal, control_c, ctrlcclean).

:- data del_on_ctrlc/2.

delete_on_ctrlc(File, Ref) :-
        working_directory(Dir, Dir),
        asserta_fact(del_on_ctrlc(Dir, File), Ref).

ctrlcclean :-
        retract_fact(del_on_ctrlc(Dir, File)),
        working_directory(_, Dir),
        delete_file(File),
        fail.
ctrlcclean :- halt.
