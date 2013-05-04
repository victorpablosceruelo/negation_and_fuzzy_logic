:- package(callback).

:- use_module(compiler(frontend), [callback__add_module_check/1,
              callback__ensure_imported/4, callback__error/0,
              callback__error/1]).

add_ensure_imported_check(M, F, A) :-
	frontend:callback__add_module_check(frontend:callback__ensure_imported(M, F, A)).

:- push_prolog_flag(multi_arity_warnings, off).

add_module_error(Error) :-
	frontend:callback__error(Error).

add_module_error :-
	frontend:callback__error.

:- pop_prolog_flag(multi_arity_warnings).
