:- use_package([]).

:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(system)).
%:- use_module(engine(internals)).

main([Op,Source,Target]) :-
        cp(Op,Source,Target).

cp('-s', Source, Target):-
	atom_codes(Source, Lso),
	atom_codes(Target, Lta),
	append("/bin/cp "||Lso, " "||Lta, Lcommand),
	atom_codes(Command, Lcommand),
	system(Command).

cp('-p', Source, Target):-
        current_input(CI),
        current_output(CO),
        open(Source, read, S),
        set_input(S),
        open(Target, write, T),
        set_output(T),
        repeat,
          get_code(C),
        ( C = -1, !
        ; put_code(C),
          fail
        ),
        set_input(CI),
        set_output(CO),
        close(S),
        close(T).
