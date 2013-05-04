% (included file)

:- use_module(engine(internals), ['$global_vars_get'/2]).

% A simple error
:- redefining(error/1).
:- static error/1.
error(Error) :-
	'$global_vars_get'(3, Errs),
	trust(Errs instance_of errlog),
	Errs.compiler_error(Error).



