:- use_package([]).

:- use_module(library(file_locks)).

main([File]):-
	lock_file(File, Fd, true),
	inform_user(['Result: ',Res,', file descriptor: ', Fd]),
	get_code(_P),
	unlock_file(Fd, _).
