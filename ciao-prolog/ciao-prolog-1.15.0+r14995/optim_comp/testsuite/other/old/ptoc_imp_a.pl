:- module(_, _, [macro]).

:- use_module(library(strings)).

:- '$pragma'(ptoc__dump).
%:- '$pragma'(ptoc__global_jump_opt).

:- '$nativedef'(a/1, ptoc([imp = semidet, register = true])).
a(X) :-
	X = 3.

:- '$native_inline'(declare(msg, pointer(char), "this is a message\n")).

% TODO: this is not working... DO SOMETHING TO CONVERT BETWEEN TAGGED_T AND INT
:- '$nativedef'(cnv/2, ptoc([imp = det,
	                     argmodes = [in, in],
			     argderefs = [false, false],
			     argmems = [x(0), push(x)],
			     argimptypes = [tagged_t, int]])).
cnv(X,Y) :-
	'$varmem'(Z, push(_z)),
	'$inline'(declare(_z, int)),
	'$inline_expr'(3, Z),
	q(Z,Z).

:- '$nativedef'(p/1, ptoc([imp = semidet, register = true])).
p(_X) :-
	% TODO: hide _x _y... how??
	'$varmem'(X, push(_x)),
	'$inline'(declare(_x, double)),
	'$varmem'(Y, push(_y)),
	'$inline'(declare(_y, static+int, 0)),
%	'$trust_type'(X, int),
	'$inline_expr'(1.0, X),
	'$inline_expr'(20+_y, Y),
	% TODO: automatically replace X by memory of X in onlyregs...
	'$varmem'(X, push(_x1)),
	'$varmem'(Y, push(_y1)),
	'$inline'(_x1 = _y1 + 1),
	q(X, Y).

:- '$nativedef'(q/2, ptoc([imp = det,
	                   argmodes = [in, in],
			   argmems = [push(x), push(y)],
			   argimptypes = [int, int]])).
q(_X,_Y) :-
	'$inline'(inline("printf(\"%s x:%d y:%d\\n\", msg, x, y)")).
