:- module(_, _, []).

:- use_module(compiler(c_itf_compat)).

test :-
	Spec = library(lists),
	Opts = [],
	get_code_and_related_assertions_opts(Spec, Opts, M, Base, Suffix, Dir),
	display(info(M, Base, Suffix, Dir)), nl,
	dump(uses_file/2),
	dump(adds/2),
	dump(includes/2),
	dump(def_multifile/4),
	dump(defines/5),
	dump(defines_module/2),
	dump(exports/5),
	dump(imports_pred/7),
	dump(assertion_read/9),
	dump(clause_read/7).

dump(F/A) :-
	functor(X, F, A),
	current_fact(X),
	display(X),
	nl,
	fail.
dump(_).
